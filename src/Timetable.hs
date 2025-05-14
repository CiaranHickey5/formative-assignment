{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Timetable
  ( Timetable(..)
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)

-- | A scheduled class entry in the timetable
data Timetable = Timetable
  { entryID      :: !String  -- ^ Unique entry ID
  , courseID     :: !String  -- ^ Course code
  , roomID       :: !String  -- ^ Room code
  , lecturerID   :: !String  -- ^ Lecturer code
  , dayOfWeek    :: !String  -- ^ Day name
  , startTime    :: !String  -- ^ Start  (HH:MM)
  , endTime      :: !String  -- ^ End    (HH:MM)
  , studentGroup :: !String  -- ^ Student group code
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

instance ToNamedRecord Timetable where
  toNamedRecord Timetable{..} = namedRecord
    [ "entryID"      .= entryID
    , "courseID"     .= courseID
    , "roomID"       .= roomID
    , "lecturerID"   .= lecturerID
    , "dayOfWeek"    .= dayOfWeek
    , "startTime"    .= startTime
    , "endTime"      .= endTime
    , "studentGroup" .= studentGroup
    ]
