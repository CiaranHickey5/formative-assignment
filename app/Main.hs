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
import           Student                (Student)
import           Allocation             (Allocation, validateAllocations)
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
  -- Read all of your CSVs
  lecs     <- readCSV "data/lecturers.csv"
  courses  <- readCSV "data/courses.csv"
  rooms    <- readCSV "data/rooms.csv"
  groups   <- readCSV "data/student_groups.csv"
  students <- readCSV "data/students.csv"
  allocs   <- readCSV "data/allocations.csv"
  tts      <- readCSV "data/timetable.csv"

  -- Validate allocations and timetable
  let allocErrs = validateAllocations lecs courses groups allocs
      ttErrs    = validateTimetableAll rooms groups lecs courses tts

  -- Write out everything (entities + errors)
  writeReports "output" lecs courses rooms groups students allocs tts

  putStrLn "Done. Reports and errors written to output/"
