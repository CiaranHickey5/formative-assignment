{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Timetable
  ( Timetable(..)
  , validateEntryId
  , validateDayOfWeek
  , validateTimeSlot
  , validateTimetable
  , isValidTimetable
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.List (isInfixOf)
import Data.Char (toLower)

data Timetable = Timetable
  { entryID      :: !String
  , courseID     :: !String
  , roomID       :: !String
  , lecturerID   :: !String
  , dayOfWeek    :: !String
  , startTime    :: !String
  , endTime      :: !String
  , studentGroup :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Timetable where
  parseNamedRecord r = Timetable
    <$> r .: "entryID"
    <*> r .: "courseID"
    <*> r .: "roomID" 
    <*> r .: "lecturerID"
    <*> r .: "dayOfWeek"
    <*> r .: "startTime"
    <*> r .: "endTime"
    <*> r .: "studentGroup"

instance ToNamedRecord Timetable

validateEntryId :: String -> ValidationResult String
validateEntryId id
  | null id                     = Invalid ["Entry ID is empty"]
  | head id /= 'T'              = Invalid ["Entry ID must start with 'T'"]
  | otherwise                   = Valid id

validateDayOfWeek :: String -> ValidationResult String
validateDayOfWeek day
  | null day                    = Invalid ["Day of week is empty"]
  | not (map toLower day `elem` validDays) = Invalid ["Invalid day of week"]
  | otherwise                   = Valid day
  where
    validDays = ["monday", "tuesday", "wednesday", "thursday", "friday"]

validateTimeSlot :: String -> String -> ValidationResult (String, String)
validateTimeSlot start end
  | null start || null end      = Invalid ["Time slot cannot be empty"]
  | not (isValidTime start)     = Invalid ["Invalid start time format (should be HH:MM)"]
  | not (isValidTime end)       = Invalid ["Invalid end time format (should be HH:MM)"]
  | start >= end                = Invalid ["Start time must be before end time"]
  | otherwise                   = Valid (start, end)
  where
    isValidTime time = 
      length time == 5 && time !! 2 == ':' &&
      all (`elem` ['0'..'9']) [time !! 0, time !! 1, time !! 3, time !! 4]

validateTimetable :: Timetable -> ValidationResult Timetable
validateTimetable entry =
  case (validateEntryId (entryID entry),
        validateDayOfWeek (dayOfWeek entry),
        validateTimeSlot (startTime entry) (endTime entry)) of
    (Valid _, Valid _, Valid _) -> Valid entry
    (idResult, dayResult, timeResult) ->
      let errors = concat [getErrors idResult "Entry ID",
                          getErrors dayResult "Day of Week",
                          getTimeErrors timeResult]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []
    getTimeErrors (Invalid errs) = map (\err -> "Time: " ++ err) errs
    getTimeErrors (Valid _) = []

isValidTimetable :: Timetable -> Bool
isValidTimetable entry =
  case validateTimetable entry of
    Valid _ -> True
    Invalid _ -> False