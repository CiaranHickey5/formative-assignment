{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Room
  ( Room(..)
  , validateRoomId
  , validateRoomName
  , validateCapacity
  , validateBuildingId
  , validateRoom
  , isValidRoom
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.Char (isAlphaNum)

data Room = Room
  { roomID     :: !String
  , roomName   :: !String
  , capacity   :: !Int
  , buildingID :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Room where
  parseNamedRecord r = Room
    <$> r .: "roomID"
    <*> r .: "name"
    <*> r .: "capacity"
    <*> r .: "buildingID"

instance ToNamedRecord Room

validateRoomId :: String -> ValidationResult String
validateRoomId id
  | null id                      = Invalid ["Room ID is empty"]
  | not (all isAlphaNum id)      = Invalid ["Room ID must contain only alphanumeric characters"]
  | otherwise                    = Valid id

validateRoomName :: String -> ValidationResult String
validateRoomName name
  | null name                    = Invalid ["Room name is empty"]
  | length name > 30             = Invalid ["Room name must be 30 characters or less"]
  | otherwise                    = Valid name

validateCapacity :: Int -> ValidationResult Int
validateCapacity cap
  | cap <= 0                     = Invalid ["Capacity must be positive"]
  | cap > 500                    = Invalid ["Capacity cannot exceed 500"]
  | otherwise                    = Valid cap

validateBuildingId :: String -> ValidationResult String
validateBuildingId buildingId
  | null buildingId              = Invalid ["Building ID is empty"]
  | otherwise                    = Valid buildingId

validateRoom :: Room -> ValidationResult Room
validateRoom room =
  case (validateRoomId (roomID room),
        validateRoomName (roomName room),
        validateCapacity (capacity room),
        validateBuildingId (buildingID room)) of
    (Valid _, Valid _, Valid _, Valid _) -> Valid room
    (idResult, nameResult, capResult, buildingResult) ->
      let errors = concat [getErrors idResult "ID",
                          getErrors nameResult "Name",
                          getErrors capResult "Capacity",
                          getErrors buildingResult "Building"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidRoom :: Room -> Bool
isValidRoom room =
  case validateRoom room of
    Valid _ -> True
    Invalid _ -> False