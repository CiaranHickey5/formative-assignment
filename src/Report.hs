{-# LANGUAGE OverloadedStrings #-}

module Report
  ( writeReports
  ) where

import Data.Csv(DefaultOrdered(..), ToNamedRecord(..), encodeDefaultOrderedByName)
import qualified Data.ByteString.Lazy as BL
import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>))
import Data.Time(getCurrentTime, formatTime, defaultTimeLocale)
import Lecturer(Lecturer)
import Module(Module)
import Room(Room)
import StudentGroup(StudentGroup)
import Allocation(Allocation)
import Timetable(Timetable)
import TimetableValidator(validateTimetableAll)
import qualified TimetableValidator as TV

-- | Write valid/invalid CSVs for each entity\ writeReports
  :: FilePath             
  -> [Lecturer] -> [Module] -> [Room] -> [StudentGroup]
  -> [Allocation] -> [Timetable]
  -> IO ()
writeReports out lecs mods rooms groups allocs tts = do
  let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M" =<< getCurrentTime
      dir       = out </> "reports-" ++ timestamp
  createDirectoryIfMissing True dir

  -- Entity reports
  BL.writeFile (dir </> "lecturers.csv")     (encodeDefaultOrderedByName lecs)
  BL.writeFile (dir </> "modules.csv")       (encodeDefaultOrderedByName mods)
  BL.writeFile (dir </> "rooms.csv")         (encodeDefaultOrderedByName rooms)
  BL.writeFile (dir </> "groups.csv")        (encodeDefaultOrderedByName groups)
  BL.writeFile (dir </> "allocations.csv")   (encodeDefaultOrderedByName allocs)
  BL.writeFile (dir </> "timetable.csv")     (encodeDefaultOrderedByName tts)

  -- Validation errors
  let allocErrs = validateAllocations lecs mods groups allocs
      ttErrs    = validateTimetableAll rooms groups lecs mods tts
  writeLines (dir </> "allocation-errors.txt") allocErrs
  writeLines (dir </> "timetable-errors.txt")    ttErrs

-- | Helper to write plain text lists
writeLines :: FilePath -> [String] -> IO ()
writeLines fp xs = writeFile fp (unlines xs)