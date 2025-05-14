-- src/Report.hs
{-# LANGUAGE OverloadedStrings #-}

module Report
  ( -- CSV report generation
    generateValidInvalidCSVs
  , writeEntityCSV
    -- Timetable report generation
  , generateLecturerTimetable
  , generateGroupTimetable
  , generateRoomTimetable
  , writeTimetableReport
  , TimetableFormat(..)
  ) where

import Lecturer
import Course
import Room
import Student
import StudentGroup
import Timetable
import Module
import Types (ValidationResult(..))
import Data.List (find)
import Data.Char (toLower)

import Data.List (groupBy, sortOn, intercalate)
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

-- Define output formats
data TimetableFormat = MarkdownFormat | TextFormat | HTMLFormat

-- Generate CSV files for valid and invalid entities
generateValidInvalidCSVs :: FilePath -> String -> [a] -> (a -> Bool) -> (a -> [String]) -> IO ()
generateValidInvalidCSVs outputDir entityName entities isValidEntity getValidationErrors = do
    -- Create output directory
    createDirectoryIfMissing True outputDir
    
    -- Split entities into valid and invalid
    let validEntities = filter isValidEntity entities
        invalidEntities = filter (not . isValidEntity) entities
    
    -- Write valid entities CSV
    writeEntityCSV (outputDir ++ "/" ++ entityName ++ "_valid.csv") validEntities
    
    -- Generate invalid entities report with errors
    let invalidReport = map (\e -> (e, getValidationErrors e)) invalidEntities
    
    -- Write invalid entities to a report file (not CSV since we need error info)
    writeInvalidReport (outputDir ++ "/" ++ entityName ++ "_invalid.md") entityName invalidReport

-- Write entities to CSV file using the ToNamedRecord instance
writeEntityCSV :: Csv.ToNamedRecord a => FilePath -> [a] -> IO ()
writeEntityCSV filePath entities = do
    let csv = Csv.encodeDefaultOrderedByName entities
    BL.writeFile filePath csv
    putStrLn $ "Wrote " ++ show (length entities) ++ " entities to " ++ filePath

-- Write invalid entities with their errors to a Markdown report
writeInvalidReport :: Show a => FilePath -> String -> [(a, [String])] -> IO ()
writeInvalidReport filePath entityName invalidEntities = do
    -- Get current time
    currentTime <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    
    -- Create report header
    let header = [ "# Invalid " ++ entityName ++ " Report"
                 , ""
                 , "Generated on: " ++ timestamp
                 , ""
                 , "Total invalid " ++ entityName ++ ": " ++ show (length invalidEntities)
                 , ""
                 , "## Details"
                 , ""
                 ]
    
    -- Format each invalid entity
    let entityReports = concatMap formatInvalidEntity invalidEntities
    
    -- Write the report
    writeFile filePath $ unlines (header ++ entityReports)
    putStrLn $ "Wrote invalid " ++ entityName ++ " report to " ++ filePath
  where
    formatInvalidEntity (entity, errors) =
      [ "### Entity"
      , ""
      , "```"
      , show entity
      , "```"
      , ""
      , "#### Validation Errors"
      , ""
      ] ++
      map (\err -> "- " ++ err) errors ++
      [""]

-- Generate a timetable report for a lecturer
generateLecturerTimetable :: TimetableFormat -> String -> [Timetable] -> [Course] -> [String]
generateLecturerTimetable format lecturerId timetables courses = 
    case format of
        MarkdownFormat -> generateLecturerMarkdown lecturerId timetables courses
        TextFormat -> generateLecturerText lecturerId timetables courses
        HTMLFormat -> generateLecturerHTML lecturerId timetables courses

-- Generate a timetable report for a student group
generateGroupTimetable :: TimetableFormat -> String -> [Timetable] -> [Course] -> [String]
generateGroupTimetable format groupId timetables courses =
    case format of
        MarkdownFormat -> generateGroupMarkdown groupId timetables courses
        TextFormat -> generateGroupText groupId timetables courses
        HTMLFormat -> generateGroupHTML groupId timetables courses

-- Generate a timetable report for a room
generateRoomTimetable :: TimetableFormat -> String -> [Timetable] -> [Course] -> [String]
generateRoomTimetable format roomId timetables courses =
    case format of
        MarkdownFormat -> generateRoomMarkdown roomId timetables courses
        TextFormat -> generateRoomText roomId timetables courses
        HTMLFormat -> generateRoomHTML roomId timetables courses

-- Write a timetable report to a file
writeTimetableReport :: FilePath -> [String] -> IO ()
writeTimetableReport filePath reportLines = do
    -- Ensure directory exists
    createDirectoryIfMissing True (takeDirectory filePath)
    
    -- Write the report
    writeFile filePath (unlines reportLines)
    putStrLn $ "Wrote timetable report to " ++ filePath

-- Markdown report generators
generateLecturerMarkdown :: String -> [Timetable] -> [Course] -> [String]
generateLecturerMarkdown lecturerId timetables courses = 
    [ "# Timetable for Lecturer: " ++ lecturerId
    , ""
    , "## Schedule"
    , ""
    , "| Day | Time | Course | Room | Group |"
    , "| --- | ---- | ------ | ---- | ----- |"
    ] ++
    map formatEntry (filter (\t -> lecturerID t == lecturerId) (sortTimetable timetables)) ++
    [ "", "---", "Generated automatically by Timetable System" ]
  where
    formatEntry t = 
        "| " ++ dayOfWeek t ++ 
        " | " ++ startTime t ++ "-" ++ endTime t ++ 
        " | " ++ getCourseNameById (courseID t) courses ++ 
        " | " ++ roomID t ++ 
        " | " ++ studentGroup t ++ " |"
    
    getCourseNameById id cs = case find (\c -> courseID c == id) cs of
                             Just c -> courseName c
                             Nothing -> id

generateGroupMarkdown :: String -> [Timetable] -> [Course] -> [String]
generateGroupMarkdown groupId timetables courses = 
    [ "# Timetable for Student Group: " ++ groupId
    , ""
    , "## Schedule"
    , ""
    , "| Day | Time | Course | Room | Lecturer |"
    , "| --- | ---- | ------ | ---- | -------- |"
    ] ++
    map formatEntry (filter (\t -> studentGroup t == groupId) (sortTimetable timetables)) ++
    [ "", "---", "Generated automatically by Timetable System" ]
  where
    formatEntry t = 
        "| " ++ dayOfWeek t ++ 
        " | " ++ startTime t ++ "-" ++ endTime t ++ 
        " | " ++ getCourseNameById (courseID t) courses ++ 
        " | " ++ roomID t ++ 
        " | " ++ lecturerID t ++ " |"
    
    getCourseNameById id cs = case find (\c -> courseID c == id) cs of
                             Just c -> courseName c
                             Nothing -> id

generateRoomMarkdown :: String -> [Timetable] -> [Course] -> [String]
generateRoomMarkdown roomId timetables courses = 
    [ "# Timetable for Room: " ++ roomId
    , ""
    , "## Schedule"
    , ""
    , "| Day | Time | Course | Group | Lecturer |"
    , "| --- | ---- | ------ | ----- | -------- |"
    ] ++
    map formatEntry (filter (\t -> roomID t == roomId) (sortTimetable timetables)) ++
    [ "", "---", "Generated automatically by Timetable System" ]
  where
    formatEntry t = 
        "| " ++ dayOfWeek t ++ 
        " | " ++ startTime t ++ "-" ++ endTime t ++ 
        " | " ++ getCourseNameById (courseID t) courses ++ 
        " | " ++ studentGroup t ++ 
        " | " ++ lecturerID t ++ " |"
    
    getCourseNameById id cs = case find (\c -> courseID c == id) cs of
                             Just c -> courseName c
                             Nothing -> id

-- Sort timetable entries by day and time
sortTimetable :: [Timetable] -> [Timetable]
sortTimetable = sortOn (\t -> (dayIndex (dayOfWeek t), startTime t))
  where
    dayIndex day = case map toLower day of
                     "monday" -> 1
                     "tuesday" -> 2
                     "wednesday" -> 3
                     "thursday" -> 4
                     "friday" -> 5
                     _ -> 6  -- Unknown days go at the end

-- Text format generators (simpler, plain text format)
generateLecturerText :: String -> [Timetable] -> [Course] -> [String]
generateLecturerText lecturerId timetables courses = 
    [ "TIMETABLE FOR LECTURER: " ++ lecturerId
    , "==============================="
    , ""
    , "DAY        TIME          COURSE                    ROOM        GROUP"
    , "-------------------------------------------------------------------------"
    ] ++
    map formatEntry (filter (\t -> lecturerID t == lecturerId) (sortTimetable timetables)) ++
    [ "", "Generated automatically by Timetable System" ]
  where
    formatEntry t = 
        padRight 10 (dayOfWeek t) ++ " " ++
        padRight 13 (startTime t ++ "-" ++ endTime t) ++ " " ++
        padRight 25 (getCourseNameById (courseID t) courses) ++ " " ++
        padRight 11 (roomID t) ++ " " ++
        studentGroup t
    
    getCourseNameById id cs = case find (\c -> courseID c == id) cs of
                             Just c -> courseName c
                             Nothing -> id
    
    padRight n s = s ++ replicate (n - length s) ' '

-- Similar implementations for generateGroupText and generateRoomText

-- HTML format generators
generateLecturerHTML :: String -> [Timetable] -> [Course] -> [String]
generateLecturerHTML lecturerId timetables courses = 
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "  <title>Timetable for Lecturer: " ++ lecturerId ++ "</title>"
    , "  <style>"
    , "    body { font-family: Arial, sans-serif; margin: 20px; }"
    , "    h1 { color: #2c3e50; }"
    , "    table { border-collapse: collapse; width: 100%; }"
    , "    th, td { padding: 8px; text-align: left; border-bottom: 1px solid #ddd; }"
    , "    th { background-color: #f2f2f2; }"
    , "    tr:hover { background-color: #f5f5f5; }"
    , "    .footer { margin-top: 20px; font-size: 0.8em; color: #7f8c8d; }"
    , "  </style>"
    , "</head>"
    , "<body>"
    , "  <h1>Timetable for Lecturer: " ++ lecturerId ++ "</h1>"
    , "  <table>"
    , "    <tr>"
    , "      <th>Day</th>"
    , "      <th>Time</th>"
    , "      <th>Course</th>"
    , "      <th>Room</th>"
    , "      <th>Group</th>"
    , "    </tr>"
    ] ++
    map formatEntry (filter (\t -> lecturerID t == lecturerId) (sortTimetable timetables)) ++
    [ "  </table>"
    , "  <div class=\"footer\">Generated automatically by Timetable System</div>"
    , "</body>"
    , "</html>"
    ]
  where
    formatEntry t = 
        "    <tr>" ++
        "<td>" ++ dayOfWeek t ++ "</td>" ++
        "<td>" ++ startTime t ++ "-" ++ endTime t ++ "</td>" ++
        "<td>" ++ getCourseNameById (courseID t) courses ++ "</td>" ++
        "<td>" ++ roomID t ++ "</td>" ++
        "<td>" ++ studentGroup t ++ "</td>" ++
        "</tr>"
    
    getCourseNameById id cs = case find (\c -> courseID c == id) cs of
                             Just c -> courseName c
                             Nothing -> id

-- Similar implementations for generateGroupHTML and generateRoomHTML