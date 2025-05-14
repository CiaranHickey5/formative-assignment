{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lecturer
  ( Lecturer(..)
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)

-- | Represents a lecturer and their available hours
data Lecturer = Lecturer
  { lecturerID     :: !String  -- ^ Unique lecturer identifier
  , lecturerName   :: !String  -- ^ Full name
  , email          :: !String  -- ^ Email address
  , departmentID   :: !String  -- ^ Dept code
  , availableHours :: !Int     -- ^ Max hours per week
  } deriving (Show, Generic)

instance FromNamedRecord Lecturer where
  parseNamedRecord r = Lecturer
    <$> r .: "lecturerID"
    <*> r .: "name"
    <*> r .: "email"
    <*> r .: "departmentID"
    <*> r .: "availableHours"

instance ToNamedRecord Lecturer where
  toNamedRecord Lecturer{..} = namedRecord
    [ "lecturerID"     .= lecturerID
    , "name"           .= lecturerName
    , "email"          .= email
    , "departmentID"   .= departmentID
    , "availableHours" .= availableHours
    ]
