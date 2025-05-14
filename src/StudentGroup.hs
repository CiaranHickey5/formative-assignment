{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module StudentGroup
  ( StudentGroup(..)
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)

-- | Represents a cohort of students
data StudentGroup = StudentGroup
  { groupID     :: !String  -- ^ Unique group code
  , groupName   :: !String  -- ^ Program name/description
  , programYear :: !Int     -- ^ Year within programme
  , groupSize   :: !Int     -- ^ Number of students
  } deriving (Show, Generic)

instance FromNamedRecord StudentGroup where
  parseNamedRecord r = StudentGroup
    <$> r .: "groupID"
    <*> r .: "name"
    <*> r .: "programYear"
    <*> r .: "size"

instance ToNamedRecord StudentGroup where
  toNamedRecord StudentGroup{..} = namedRecord
    [ "groupID"     .= groupID
    , "name"        .= groupName
    , "programYear" .= programYear
    , "size"        .= groupSize
    ]
