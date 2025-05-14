{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Module
  ( Module(..)
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)

-- | Represents a module/course with its credit hours
data Module = Module
  { courseID    :: !String  -- ^ Unique course identifier
  , moduleHours :: !Int     -- ^ Number of credit/contact hours
  } deriving (Show, Generic)

instance FromNamedRecord Module where
  parseNamedRecord r = Module
    <$> r .: "courseID"
    <*> r .: "credits"

instance ToNamedRecord Module where
  toNamedRecord Module{..} = namedRecord
    [ "courseID" .= courseID
    , "credits"  .= moduleHours
    ]
