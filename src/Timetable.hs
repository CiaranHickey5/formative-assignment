{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Timetable
  ( Timetable(..)
  , validateTimetable
  , validateTimetableSystem
  , isValidTimetable
  ) where

import Types (ValidationResult(..))
import Lecturer (Lecturer(..))
import Module (Module(..))
import Room (Room(..))
import StudentGroup (StudentGroup(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.List (groupBy, sortOn)
import Data.Function (on)

data Timetable = Timetable
  { timeSlot     :: !String
  , moduleName   :: !String
  , groupId      :: !String
  , roomId       :: !String
  , lecturerName :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Timetable where
  parseNamedRecord r = Timetable
    <$> r .: "Time"
    <*> r .: "module-name"
    <*> r .: "group-id"
    <*> r .: "room-id"
    <*> r .: "lecturer-name"

instance ToNamedRecord Timetable

-- Basic timetable entry validation
validateTimetable :: Timetable -> ValidationResult Timetable
validateTimetable entry =
  -- Simple validation of time slot format (could be enhanced)
  if null (timeSlot entry)
    then Invalid ["Time slot cannot be empty"]
    else Valid entry

-- System-level timetable validation
validateTimetableSystem :: [Lecturer] -> [Module] -> [Room] -> [StudentGroup] -> [Timetable] -> [String]
validateTimetableSystem lecturers modules rooms groups timetable =
  concat [
    checkRoomDoubleBookings timetable,
    checkLecturerDoubleBookings timetable,
    checkGroupDoubleBookings timetable,
    checkRoomCapacity rooms groups timetable,
    checkModuleHoursScheduled modules timetable
  ]

-- Check for room double bookings
checkRoomDoubleBookings :: [Timetable] -> [String]
checkRoomDoubleBookings timetable =
  let
    -- Group by time slot
    timeGroups = groupBy ((==) `on` timeSlot) (sortOn timeSlot timetable)
    
    -- Check each time group for room duplicates
    conflicts = concatMap findRoomConflicts timeGroups
  in
    conflicts
  where
    findRoomConflicts timeGroup =
      let
        -- Group by room within the time group
        roomGroups = groupBy ((==) `on` roomId) (sortOn roomId timeGroup)
        
        -- Find groups with more than one entry (conflicts)
        conflictGroups = filter (\g -> length g > 1) roomGroups
      in
        map formatConflict conflictGroups
    
    formatConflict (conflict:_) =
      "Room double booking: " ++ roomId conflict ++ " at " ++ timeSlot conflict

-- Check for lecturer double bookings
checkLecturerDoubleBookings :: [Timetable] -> [String]
checkLecturerDoubleBookings timetable =
  let
    -- Group by time slot
    timeGroups = groupBy ((==) `on` timeSlot) (sortOn timeSlot timetable)
    
    -- Check each time group for lecturer duplicates
    conflicts = concatMap findLecturerConflicts timeGroups
  in
    conflicts
  where
    findLecturerConflicts timeGroup =
      let
        -- Group by lecturer within the time group
        lecturerGroups = groupBy ((==) `on` lecturerName) (sortOn lecturerName timeGroup)
        
        -- Find groups with more than one entry (conflicts)
        conflictGroups = filter (\g -> length g > 1) lecturerGroups
      in
        map formatConflict conflictGroups
    
    formatConflict (conflict:_) =
      "Lecturer double booking: " ++ lecturerName conflict ++ " at " ++ timeSlot conflict

-- Check for student group double bookings
checkGroupDoubleBookings :: [Timetable] -> [String]
checkGroupDoubleBookings timetable =
  let
    -- Group by time slot
    timeGroups = groupBy ((==) `on` timeSlot) (sortOn timeSlot timetable)
    
    -- Check each time group for group duplicates
    conflicts = concatMap findGroupConflicts timeGroups
  in
    conflicts
  where
    findGroupConflicts timeGroup =
      let
        -- Group by student group within the time group
        groupGroups = groupBy ((==) `on` groupId) (sortOn groupId timeGroup)
        
        -- Find groups with more than one entry (conflicts)
        conflictGroups = filter (\g -> length g > 1) groupGroups
      in
        map formatConflict conflictGroups
    
    formatConflict (conflict:_) =
      "Student group double booking: " ++ groupId conflict ++ " at " ++ timeSlot conflict

-- Check if rooms are large enough for the scheduled student groups
checkRoomCapacity :: [Room] -> [StudentGroup] -> [Timetable] -> [String]
checkRoomCapacity rooms groups timetable =
  concatMap checkCapacity timetable
  where
    checkCapacity entry =
      let
        room = find (\r -> roomId r == roomId entry) rooms
        group = find (\g -> groupId g == groupId entry) groups
      in
        case (room, group) of
          (Just r, Just g) -> 
            if capacity r < groupSize g
            then ["Room capacity exceeded: Room " ++ roomId entry ++ 
                  " has capacity " ++ show (capacity r) ++ 
                  " but group " ++ groupId entry ++ 
                  " has size " ++ show (groupSize g)]
            else []
          _ -> []

-- Check if all module hours are scheduled
checkModuleHoursScheduled :: [Module] -> [Timetable] -> [String]
checkModuleHoursScheduled modules timetable =
  let
    -- Group timetable by module
    moduleGroups = groupBy ((==) `on` moduleName) (sortOn moduleName timetable)
    
    -- Check each module's scheduled hours
    moduleChecks = map checkModuleHours moduleGroups
  in
    concat moduleChecks
  where
    checkModuleHours [] = []
    checkModuleHours modGroup@(first:_) =
      let
        modName = moduleName first
        scheduledCount = length modGroup  -- Simplified: assume 1 hour per slot
        
        -- Find the module from the modules list
        mod = find (\m -> moduleName m == modName) modules
      in
        case mod of
          Nothing -> ["Module not found: " ++ modName]
          Just m ->
            if scheduledCount < moduleHours m
              then ["Module " ++ modName ++ " requires " ++ 
                    show (moduleHours m) ++ " hours but only " ++ 
                    show scheduledCount ++ " are scheduled"]
              else if scheduledCount > moduleHours m
                   then ["Module " ++ modName ++ " requires " ++ 
                         show (moduleHours m) ++ " hours but " ++ 
                         show scheduledCount ++ " are scheduled"]
                   else []

isValidTimetable :: Timetable -> Bool
isValidTimetable entry =
  case validateTimetable entry of
    Valid _ -> True
    Invalid _ -> False