{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TimetableValidator
  ( validateTimetableAll
  , overScheduled
  ) where

import qualified Timetable as T
import Lecturer (Lecturer(..))
import Module (Module(..))
import Room (Room(..))
import StudentGroup (StudentGroup(..))
import Data.List (groupBy, sortOn, find)
import Data.Function (on)

-- | Parse "HH:MM" to an integer hour (0-23)
parseHour :: String -> Int
parseHour s = case break (==':') s of
  (h, ':' : _) -> case reads h of
                    [(n,"")] -> n
                    _          -> 0
  _            -> 0

-- | Count scheduled slots per lecturer; 1.0 per slot
lecturerSchedules :: [T.Timetable] -> [(String, Float)]
lecturerSchedules tts =
  let grouped = groupBy ((==) `on` T.lecturerID) (sortOn T.lecturerID tts)
  in [ ( key, fromIntegral (length grp) )
     | grp@((T.Timetable{ T.lecturerID = key }):_) <- grouped
     ]

-- | Lecturers exceeding availability
overScheduled :: [Lecturer] -> [T.Timetable] -> [(Lecturer, Float)]
overScheduled lecs tts =
  let sched = lecturerSchedules tts
  in [ (lec, used)
     | lec@Lecturer{ lecturerID = lid, availableHours = avail } <- lecs
     , Just used <- [lookup lid sched]
     , used > fromIntegral avail
     ]

-- | Run room, lecturer, student-group clashes and module-hour checks
validateTimetableAll
  :: [Room] -> [StudentGroup] -> [Lecturer] -> [Module] -> [T.Timetable] -> [String]
validateTimetableAll rooms _groups _lecs mods tts =
  concat
    [ checkRoomDoubleBookings     tts
    , checkLecturerDoubleBookings  tts
    , checkGroupDoubleBookings     tts
    , checkModuleHoursScheduled    mods tts
    ]

-- Room double-bookings
checkRoomDoubleBookings :: [T.Timetable] -> [String]
checkRoomDoubleBookings tts =
  let slots = groupBy ((==) `on` \x -> (T.dayOfWeek x, T.startTime x))
                     (sortOn (\x -> (T.dayOfWeek x, T.startTime x)) tts)
  in [ "Room double booking: " ++ T.roomID e
       ++ " at " ++ T.dayOfWeek e ++ " " ++ T.startTime e
     | slot <- slots, length slot > 1, e <- take 1 slot
     ]

-- Lecturer double-bookings
checkLecturerDoubleBookings :: [T.Timetable] -> [String]
checkLecturerDoubleBookings tts =
  let slots = groupBy ((==) `on` \x -> (T.dayOfWeek x, T.startTime x))
                     (sortOn (\x -> (T.dayOfWeek x, T.startTime x)) tts)
  in [ "Lecturer double booking: " ++ T.lecturerID e
       ++ " at " ++ T.dayOfWeek e ++ " " ++ T.startTime e
     | slot <- slots, length slot > 1, e <- take 1 slot
     ]

-- Student-group double-bookings
checkGroupDoubleBookings :: [T.Timetable] -> [String]
checkGroupDoubleBookings tts =
  let slots = groupBy ((==) `on` \x -> (T.dayOfWeek x, T.startTime x))
                     (sortOn (\x -> (T.dayOfWeek x, T.startTime x)) tts)
  in [ "Student group double booking: " ++ T.studentGroup e
       ++ " at " ++ T.dayOfWeek e ++ " " ++ T.startTime e
     | slot <- slots, length slot > 1, e <- take 1 slot
     ]

-- Module hours coverage
checkModuleHoursScheduled :: [Module] -> [T.Timetable] -> [String]
checkModuleHoursScheduled mods tts =
  let byMod = groupBy ((==) `on` T.courseID) (sortOn T.courseID tts)
  in concat
       [ let name     = T.courseID (head grp)
             actual   = length grp
             required = maybe (-1) moduleHours (find ((== name) . courseID) mods)
         in if actual /= required
            then [ "Module " ++ name
                   ++ " requires " ++ show required
                   ++ " hours but " ++ show actual
                   ++ " scheduled" ]
            else []
       | grp <- byMod
       ]