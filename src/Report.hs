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
import           Student                      (Student)
import           Allocation                   (Allocation)
import           Timetable                    (Timetable)
import           TimetableValidator           (validateTimetableAll)
import           Allocation                   (validateAllocations)

-- | Write entity CSVs and a timelog of validation errors.
writeReports
  :: FilePath                           -- ^ output directory
  -> [Lecturer]
  -> [Module]
  -> [Room]
  -> [StudentGroup]
  -> [Student]
  -> [Allocation]
  -> [Timetable]
  -> IO ()
writeReports out lecs mods rooms groups students allocs tts = do
  timestamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M" <$> getCurrentTime
  let dir = out </> ("reports-" ++ timestamp)
  createDirectoryIfMissing True dir

  -- Lecturers
  let lecHeader = fromList ["lecturerID","name","email","departmentID","availableHours"]
  BL.writeFile (dir </> "lecturers.csv")
               (encodeByName lecHeader (map toNamedRecord lecs))

  -- Modules
  let modHeader = fromList ["courseID","credits"]
  BL.writeFile (dir </> "modules.csv")
               (encodeByName modHeader (map toNamedRecord mods))

  -- Rooms
  let roomHeader = fromList ["roomID","name","capacity","buildingID"]
  BL.writeFile (dir </> "rooms.csv")
               (encodeByName roomHeader (map toNamedRecord rooms))

  -- Student Groups
  let grpHeader = fromList ["groupID","name","programYear","size"]
  BL.writeFile (dir </> "student_groups.csv")
               (encodeByName grpHeader (map toNamedRecord groups))

  -- Students
  let stuHeader = fromList ["studentID","name","email","programID"]
  BL.writeFile (dir </> "students.csv")
               (encodeByName stuHeader (map toNamedRecord students))

  -- Allocations
  let allocHeader = fromList ["lecturerID","courseID","studentGroup","allocHours"]
  BL.writeFile (dir </> "allocations.csv")
               (encodeByName allocHeader (map toNamedRecord allocs))

  -- Timetable
  let ttHeader = fromList
        [ "entryID","courseID","roomID","lecturerID"
        , "dayOfWeek","startTime","endTime","studentGroup"
        ]
  BL.writeFile (dir </> "timetable.csv")
               (encodeByName ttHeader (map toNamedRecord tts))

  -- Validation errors
  let allocErrs = validateAllocations lecs mods groups allocs
      ttErrs    = validateTimetableAll rooms groups lecs mods tts
      errs      = allocErrs ++ ttErrs

  writeLines (dir </> "timelog.txt") errs

-- | Helper to write plain-text lists
writeLines :: FilePath -> [String] -> IO ()
writeLines fp xs = writeFile fp (unlines xs)
