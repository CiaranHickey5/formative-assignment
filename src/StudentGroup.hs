{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StudentGroup
  ( StudentGroup(..)
  , validateGroupId
  , validateGroupName
  , validateProgramYear
  , validateGroupSize
  , validateStudentGroup
  , isValidStudentGroup
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv

data StudentGroup = StudentGroup
  { groupID     :: !String
  , groupName   :: !String
  , programYear :: !Int
  , groupSize   :: !Int
  } deriving (Show, Generic)

instance FromNamedRecord StudentGroup where
  parseNamedRecord r = StudentGroup
    <$> r .: "groupID"
    <*> r .: "name"
    <*> r .: "programYear"
    <*> r .: "size"

instance ToNamedRecord StudentGroup

validateGroupId :: String -> ValidationResult String
validateGroupId id
  | null id                      = Invalid ["Group ID is empty"]
  | head id /= 'G'               = Invalid ["Group ID must start with 'G'"]
  | otherwise                    = Valid id

validateGroupName :: String -> ValidationResult String
validateGroupName name
  | null name                    = Invalid ["Group name is empty"]
  | length name > 50             = Invalid ["Group name must be 50 characters or less"]
  | otherwise                    = Valid name

validateProgramYear :: Int -> ValidationResult Int
validateProgramYear year
  | year < 1                     = Invalid ["Program year must be positive"]
  | year > 5                     = Invalid ["Program year cannot exceed 5"]
  | otherwise                    = Valid year

validateGroupSize :: Int -> ValidationResult Int
validateGroupSize size
  | size <= 0                    = Invalid ["Group size must be positive"]
  | size > 500                   = Invalid ["Group size cannot exceed 500"]
  | otherwise                    = Valid size

validateStudentGroup :: StudentGroup -> ValidationResult StudentGroup
validateStudentGroup group =
  case (validateGroupId (groupID group),
        validateGroupName (groupName group),
        validateProgramYear (programYear group),
        validateGroupSize (groupSize group)) of
    (Valid _, Valid _, Valid _, Valid _) -> Valid group
    (idResult, nameResult, yearResult, sizeResult) ->
      let errors = concat [getErrors idResult "Group ID",
                          getErrors nameResult "Group Name",
                          getErrors yearResult "Program Year",
                          getErrors sizeResult "Group Size"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidStudentGroup :: StudentGroup -> Bool
isValidStudentGroup group =
  case validateStudentGroup group of
    Valid _ -> True
    Invalid _ -> False