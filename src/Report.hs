{-# LANGUAGE OverloadedStrings #-}

module Report
  ( generateLecturerReport
  , generateTimetableReport
  , generateValidationReport
  , writeReport
  ) where

import Lecturer
import Course
import Room
import Student
import StudentGroup
import Timetable
import Types (ValidationResult(..))
import Data.List (intercalate, sortOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)

-- Generate a Markdown report for lecturers
generateLecturerReport :: [Lecturer] -> [String]
generateLecturerReport lecturers = 
  [ "# Lecturer Report"
  , ""
  , "Generated on: " ++ "PLACEHOLDER_DATE"
  , ""
  , "## Summary"
  , ""
  , "Total Lecturers: " ++ show (length lecturers)
  , ""
  , "## Lecturer Details"
  , ""
  ] ++ 
  concatMap formatLecturer (sortOn name lecturers) ++
  [ ""
  , "---"
  , "End of Report"
  ]
  where
    formatLecturer l = 
      [ "### " ++ name l
      , ""
      , "- **ID**: " ++ lecturerID l
      , "- **Email**: " ++ email l
      , "- **Department**: " ++ departmentID l
      , "- **Available Hours**: " ++ show (availableHours l)
      , ""
      ]

-- Generate a Markdown report for the timetable
generateTimetableReport :: [Timetable] -> [Lecturer] -> [Course] -> [Room] -> [StudentGroup] -> [String]
generateTimetableReport timetableEntries lecturers courses rooms groups =
  [ "# Timetable Report"
  , ""
  , "Generated on: " ++ "PLACEHOLDER_DATE"
  , ""
  , "## Summary"
  , ""
  , "Total Entries: " ++ show (length timetableEntries)
  , ""
  , "## Timetable by Day"
  ]
  ++ generateDailyTimetables timetableEntries lecturers courses rooms groups
  ++ [ ""
     , "---"
     , "End of Report"
     ]
  where
    generateDailyTimetables entries lects crs rms grps =
      let
        days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
        dayReports = concatMap (\day -> 
                                  [ "", "### " ++ day, "" ] ++ 
                                  formatDayEntries day entries lects crs rms grps) days
      in
        dayReports
    
    formatDayEntries day entries lects crs rms grps =
      let
        dayEntries = filter (\e -> dayOfWeek e == day) entries
      in
        if null dayEntries
        then ["No entries for this day."]
        else ["| Time | Course | Room | Lecturer | Group |", 
              "| ---- | ------ | ---- | -------- | ----- |"] ++
             map (formatEntry lects crs rms grps) (sortOn startTime dayEntries)
    
    formatEntry lects crs rms grps entry =
      let
        timeSlot = startTime entry ++ " - " ++ endTime entry
        courseName = findCourseName (courseID entry) crs
        roomName = findRoomName (roomID entry) rms
        lecturerName = findLecturerName (lecturerID entry) lects
        groupName = findGroupName (studentGroup entry) grps
      in
        "| " ++ timeSlot ++ " | " ++ courseName ++ " | " ++ roomName ++ " | " ++ lecturerName ++ " | " ++ groupName ++ " |"
    
    findCourseName id cs = maybe id courseName $ find (\c -> courseID c == id) cs
    findRoomName id rs = maybe id roomName $ find (\r -> roomID r == id) rs
    findLecturerName id ls = maybe id name $ find (\l -> lecturerID l == id) ls
    findGroupName id gs = maybe id groupName $ find (\g -> groupID g == id) gs

-- Generate a report on validation results
generateValidationReport :: [(String, [String])] -> [String]
generateValidationReport validationResults = 
  [ "# Validation Report"
  , ""
  , "Generated on: " ++ "PLACEHOLDER_DATE" 
  , ""
  , "## Summary"
  , ""
  , "Total Issues Found: " ++ show (sum (map (length . snd) validationResults))
  , ""
  , "## Validation Issues"
  , ""
  ] ++
  concatMap formatValidationResults validationResults ++
  [ ""
  , "---"
  , "End of Report"
  ]
  where
    formatValidationResults (category, issues) =
      [ "### " ++ category
      , ""
      ] ++
      (if null issues 
       then ["No issues found."]
       else map (\issue -> "- " ++ issue) issues) ++
      [""]

-- Write a report to a file
writeReport :: FilePath -> [String] -> IO ()
writeReport filePath report = do
  -- Get current time for the report
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  
  -- Replace placeholder with actual date
  let reportWithDate = map (\line -> if line == "Generated on: PLACEHOLDER_DATE"
                                    then "Generated on: " ++ formattedTime
                                    else line) report
  
  -- Ensure the directory exists
  let dir = takeDirectory filePath
  createDirectoryIfMissing True dir
  
  -- Write to file
  writeFile filePath (unlines reportWithDate)
  putStrLn $ "Report written to " ++ filePath