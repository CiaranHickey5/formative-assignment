{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Csv                (decodeByName)
import qualified Data.ByteString.Lazy    as BL
import           System.Exit             (die)

import           Lecturer                (Lecturer)
import           Module                  (Module)
import           Room                    (Room)
import           StudentGroup            (StudentGroup)
import           Allocation              (Allocation, validateAllocations)
import           Timetable               (Timetable)
import           TimetableValidator      (validateTimetableAll)
import           Report                  (writeReports)

-- | Read a named CSV and return parsed rows or error
readCSV :: FromNamedRecord a => FilePath -> IO [a]
readCSV file = do
  bs <- BL.readFile file
  case decodeByName bs of
    Left err      -> die $ "CSV parse error in " ++ file ++ ": " ++ err
    Right (_, xs) -> return (toList xs)

main :: IO ()
main = do
  lecs   <- readCSV "data/lecturers.csv"
  mods   <- readCSV "data/modules.csv"
  rooms  <- readCSV "data/rooms.csv"
  groups <- readCSV "data/groups.csv"
  allocs <- readCSV "data/allocations.csv"
  tts    <- readCSV "data/timetable.csv"

  let allocErrs = validateAllocations lecs mods groups allocs
      ttErrs    = validateTimetableAll rooms groups lecs mods tts

  writeReports "output" lecs mods rooms groups allocs tts

  putStrLn "Done. Reports and errors written to output/"