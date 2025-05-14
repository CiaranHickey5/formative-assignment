{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Room
  ( Room(..)
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)

-- | Represents a teaching room
data Room = Room
  { roomID     :: !String  -- ^ Unique room code 
  , roomName   :: !String  -- ^ Room name
  , capacity   :: !Int     -- ^ Max seating capacity
  , buildingID :: !String  -- ^ Building code
  } deriving (Show, Generic)

instance FromNamedRecord Room where
  parseNamedRecord r = Room
    <$> r .: "roomID"
    <*> r .: "name"
    <*> r .: "capacity"
    <*> r .: "buildingID"

instance ToNamedRecord Room where
  toNamedRecord Room{..} = namedRecord
    [ "roomID"     .= roomID
    , "name"       .= roomName
    , "capacity"   .= capacity
    , "buildingID" .= buildingID
    ]
