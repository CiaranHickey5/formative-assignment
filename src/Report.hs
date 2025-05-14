{-# LANGUAGE OverloadedStrings #-}

module Report
  ( writeReports
  ) where

import           Data.Csv                     (ToNamedRecord(toNamedRecord), encodeByName)
import qualified Data.ByteString.Lazy         as BL
import           Data.Vector                  (fromList)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              ((</>))
import           Data.Time                    (getCurrentTime, formatTime, defaultTimeLocale)
import           Lecturer                     (Lecturer)
import           Module                       (Module)
import           Room                         (Room)
import           StudentGroup                 (StudentGroup)
import           Allocation                   (Allocation)
import           Timetable                    (Timetable)
import           TimetableValidator           (validateTimetableAll)

-- | Write valid/invalid CSVs for each entity
writeReports
  :: FilePath                           -- ^ output directory
  -> [Lecturer] -> [Module] -> [Room] -> [StudentGroup]
  -> [Allocation] -> [Timetable]
  -> IO ()
writeReports out lecs mods rooms groups allocs tts = do
  timestamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M" <$> getCurrentTime
  let dir = out </> ("reports-" ++ timestamp)
  createDirectoryIfMissing True dir

  -- Lecturers
  let lecHeader = fromList ["lecturerID","name","email","departmentID","availableHours"]
      lecRecs   = map toNamedRecord lecs
  BL.writeFile (dir </> "lecturers.csv")   (encodeByName lecHeader lecRecs)

  -- Modules
  let modHeader = fromList ["courseID","credits"]
      modRecs   = map toNamedRecord mods
  BL.writeFile (dir </> "modules.csv")     (encodeByName modHeader modRecs)

  -- Rooms
  let roomHeader = fromList ["roomID","name","capacity","buildingID"]
      roomRecs   = map toNamedRecord rooms
  BL.writeFile (dir </> "rooms.csv")       (encodeByName roomHeader roomRecs)

  -- Student groups
  let grpHeader = fromList ["groupID","name","programYear","size"]
      grpRecs   = map toNamedRecord groups
  BL.writeFile (dir </> "groups.csv")      (encodeByName grpHeader grpRecs)

  -- Allocations
  let allocHeader = fromList ["lecturerID","courseID","studentGroup","allocHours"]
      allocRecs   = map toNamedRecord allocs
  BL.writeFile (dir </> "allocations.csv") (encodeByName allocHeader allocRecs)

  -- Timetable
  let ttHeader = fromList
        [ "entryID","courseID","roomID","lecturerID"
        , "dayOfWeek","startTime","endTime","studentGroup"
        ]
      ttRecs = map toNamedRecord tts
  BL.writeFile (dir </> "timetable.csv")   (encodeByName ttHeader ttRecs)

  -- Validation errors
  let errs = validateTimetableAll rooms groups [] mods tts
  writeLines (dir </> "timelog.txt") errs

-- | Helper to write plain text lists
writeLines :: FilePath -> [String] -> IO ()
writeLines fp xs = writeFile fp (unlines xs)
