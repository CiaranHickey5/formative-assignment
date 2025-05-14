{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Csv               (decodeByName, FromNamedRecord)
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (toList)
import           System.Exit            (die)

import           Lecturer               (Lecturer)
import           Module                 (Module)
import           Room                   (Room)
import           StudentGroup           (StudentGroup)
import           Allocation             (Allocation)   -- needed for type
import           Timetable              (Timetable)
import           TimetableValidator     (validateTimetableAll)
import           Report                 (writeReports)

-- | Read a named CSV file and return parsed records or exit on parse error
readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV file = do
  bs <- BL.readFile file
  case decodeByName bs of
    Left err      -> die $ "CSV parse error in " ++ file ++ ": " ++ err
    Right (_, xs) -> return (toList xs)

main :: IO ()
main = do
  lecs    <- readCSV "data/lecturers.csv"
  courses <- readCSV "data/courses.csv"
  rooms   <- readCSV "data/rooms.csv"
  groups  <- readCSV "data/student_groups.csv"
  -- stub allocations until you have an allocations.csv
  let allocs = [] :: [Allocation]

  tts     <- readCSV "data/timetable.csv"

  -- run timetable validation (allocations not yet used here)
  let ttErrs = validateTimetableAll rooms groups lecs courses tts

  -- write out all reports (including empty allocations list)
  writeReports "output" lecs courses rooms groups allocs tts

  putStrLn "Done. Reports and errors written to output/"
