module TimetableValidator
  ( ValidationIssue(..)
  , validateTimetableSystem
  , checkLecturerOverscheduling
  , checkRoomDoubleBookings
  , checkRoomCapacity
  , checkStudentGroupOverlaps
  ) where

import Lecturer
import Course
import Room
import Student
import StudentGroup
import Timetable
import Types (ValidationResult(..))
import Data.List (groupBy, sortOn, nubBy)
import Data.Function (on)
import Data.Maybe (maybeToList)

data ValidationIssue = ValidationIssue
  { issueType :: String
  , issueDescription :: String
  } deriving (Show)

-- Main validation function
validateTimetableSystem :: [Lecturer] -> [Course] -> [Room] -> [StudentGroup] -> [Timetable] -> [ValidationIssue]
validateTimetableSystem lecturers courses rooms groups timetable =
  checkLecturerOverscheduling lecturers timetable ++
  checkRoomDoubleBookings timetable ++
  checkRoomCapacity rooms groups timetable ++
  checkStudentGroupOverlaps timetable ++
  checkEntityReferences lecturers courses rooms groups timetable

-- Check if lecturers are scheduled for more hours than they're available
checkLecturerOverscheduling :: [Lecturer] -> [Timetable] -> [ValidationIssue]
checkLecturerOverscheduling lecturers timetable = 
  let 
    lecturerSchedules = map (\l -> (l, scheduledHours l)) lecturers
    overScheduled = filter (\(l, hours) -> hours > availableHours l) lecturerSchedules
  in
    map formatOverschedulingIssue overScheduled
  where
    scheduledHours lecturer = 
      let 
        id = lecturerID lecturer
        lecturerEntries = filter (\entry -> lecturerID entry == id) timetable
      in
        sum (map calculateEntryHours lecturerEntries)
    
    calculateEntryHours entry =
      let
        startHour = parseHour (startTime entry)
        endHour = parseHour (endTime entry)
      in
        endHour - startHour
    
    parseHour timeStr =
      let
        hour = read (take 2 timeStr) :: Float
        minute = read (drop 3 timeStr) :: Float
      in
        hour + (minute / 60)
    
    formatOverschedulingIssue (lecturer, hours) =
      ValidationIssue
        { issueType = "Lecturer Overscheduling"
        , issueDescription = "Lecturer " ++ name lecturer ++ " (ID: " ++ lecturerID lecturer ++ 
                             ") is scheduled for " ++ show hours ++ " hours but is only available for " ++ 
                             show (availableHours lecturer) ++ " hours."
        }

-- Check for room double bookings
checkRoomDoubleBookings :: [Timetable] -> [ValidationIssue]
checkRoomDoubleBookings timetable =
  concatMap checkOverlaps (groupBy ((==) `on` roomID) (sortOn roomID timetable))
  where
    checkOverlaps [] = []
    checkOverlaps [_] = []
    checkOverlaps roomEntries@(firstEntry:_) =
      let
        roomId = roomID firstEntry
        dayGroups = groupBy ((==) `on` dayOfWeek) (sortOn dayOfWeek roomEntries)
        overlaps = concatMap findOverlaps dayGroups
      in
        map (formatDoubleBookingIssue roomId) overlaps
    
    findOverlaps dayEntries@(firstEntry:_) =
      let
        day = dayOfWeek firstEntry
        timeOverlaps = findTimeOverlaps (sortOn startTime dayEntries)
      in
        map (\(e1, e2) -> (day, e1, e2)) timeOverlaps
    
    findTimeOverlaps [] = []
    findTimeOverlaps [_] = []
    findTimeOverlaps (e1:e2:rest) =
      let
        overlap = isTimeOverlap e1 e2
        restOverlaps = findTimeOverlaps (e2:rest)
      in
        if overlap then (e1, e2) : restOverlaps else restOverlaps
    
    isTimeOverlap e1 e2 =
      startTime e2 < endTime e1
    
    formatDoubleBookingIssue roomId (day, e1, e2) =
      ValidationIssue
        { issueType = "Room Double Booking"
        , issueDescription = "Room " ++ roomId ++ " is double-booked on " ++ day ++ 
                             ". Entry " ++ entryID e1 ++ " (" ++ startTime e1 ++ "-" ++ endTime e1 ++ 
                             ") overlaps with Entry " ++ entryID e2 ++ " (" ++ startTime e2 ++ "-" ++ endTime e2 ++ ")."
        }

-- Check if rooms are large enough for the scheduled student groups
checkRoomCapacity :: [Room] -> [StudentGroup] -> [Timetable] -> [ValidationIssue]
checkRoomCapacity rooms groups timetable =
  concatMap checkCapacity timetable
  where
    checkCapacity entry =
      let
        room = find (\r -> roomID r == roomID entry) rooms
        group = find (\g -> groupID g == studentGroup entry) groups
      in
        case (room, group) of
          (Just r, Just g) -> 
            if capacity r < groupSize g
            then [ValidationIssue
                   { issueType = "Room Capacity Exceeded"
                   , issueDescription = "Room " ++ roomName r ++ " (ID: " ++ roomID r ++ 
                                       ") has capacity " ++ show (capacity r) ++ 
                                       " but is scheduled for group " ++ groupName g ++ 
                                       " (ID: " ++ groupID g ++ ") with size " ++ show (groupSize g) ++ 
                                       " in entry " ++ entryID entry ++ "."
                   }]
            else []
          _ -> []

-- Check if student groups are scheduled for overlapping time slots
checkStudentGroupOverlaps :: [Timetable] -> [ValidationIssue]
checkStudentGroupOverlaps timetable =
  concatMap checkOverlaps (groupBy ((==) `on` studentGroup) (sortOn studentGroup timetable))
  where
    checkOverlaps [] = []
    checkOverlaps [_] = []
    checkOverlaps groupEntries@(firstEntry:_) =
      let
        groupId = studentGroup firstEntry
        dayGroups = groupBy ((==) `on` dayOfWeek) (sortOn dayOfWeek groupEntries)
        overlaps = concatMap findOverlaps dayGroups
      in
        map (formatGroupOverlapIssue groupId) overlaps
    
    findOverlaps dayEntries@(firstEntry:_) =
      let
        day = dayOfWeek firstEntry
        timeOverlaps = findTimeOverlaps (sortOn startTime dayEntries)
      in
        map (\(e1, e2) -> (day, e1, e2)) timeOverlaps
    
    findTimeOverlaps [] = []
    findTimeOverlaps [_] = []
    findTimeOverlaps (e1:e2:rest) =
      let
        overlap = isTimeOverlap e1 e2
        restOverlaps = findTimeOverlaps (e2:rest)
      in
        if overlap then (e1, e2) : restOverlaps else restOverlaps
    
    isTimeOverlap e1 e2 =
      startTime e2 < endTime e1
    
    formatGroupOverlapIssue groupId (day, e1, e2) =
      ValidationIssue
        { issueType = "Student Group Overlap"
        , issueDescription = "Student Group " ++ groupId ++ " is double-booked on " ++ day ++ 
                             ". Entry " ++ entryID e1 ++ " (" ++ startTime e1 ++ "-" ++ endTime e1 ++ 
                             ") overlaps with Entry " ++ entryID e2 ++ " (" ++ startTime e2 ++ "-" ++ endTime e2 ++ ")."
        }

-- Check that all referenced entities exist in the system
checkEntityReferences :: [Lecturer] -> [Course] -> [Room] -> [StudentGroup] -> [Timetable] -> [ValidationIssue]
checkEntityReferences lecturers courses rooms groups timetable =
  concatMap checkEntry timetable
  where
    checkEntry entry =
      let
        lecturerExists = any (\l -> lecturerID l == lecturerID entry) lecturers
        courseExists = any (\c -> courseID c == courseID entry) courses
        roomExists = any (\r -> roomID r == roomID entry) rooms
        groupExists = any (\g -> groupID g == studentGroup entry) groups
        
        issues = concat [
          if not lecturerExists then [ValidationIssue 
                                      { issueType = "Missing Reference"
                                      , issueDescription = "Timetable entry " ++ entryID entry ++ 
                                                         " references non-existent lecturer ID: " ++ lecturerID entry
                                      }] else [],
          if not courseExists then [ValidationIssue 
                                   { issueType = "Missing Reference"
                                   , issueDescription = "Timetable entry " ++ entryID entry ++ 
                                                      " references non-existent course ID: " ++ courseID entry
                                   }] else [],
          if not roomExists then [ValidationIssue 
                                 { issueType = "Missing Reference"
                                 , issueDescription = "Timetable entry " ++ entryID entry ++ 
                                                    " references non-existent room ID: " ++ roomID entry
                                 }] else [],
          if not groupExists then [ValidationIssue 
                                  { issueType = "Missing Reference"
                                  , issueDescription = "Timetable entry " ++ entryID entry ++ 
                                                     " references non-existent student group ID: " ++ studentGroup entry
                                  }] else []
          ]
      in
        issues