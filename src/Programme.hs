{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Programme
  ( Programme(..)
  , validateProgrammeId
  , validateProgrammeName
  , validateProgramme
  , isValidProgramme
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv

data Programme = Programme
  { programmeId   :: !String
  , programmeName :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Programme where
  parseNamedRecord r = Programme
    <$> r .: "prog-id"
    <*> r .: "prog-name"

instance ToNamedRecord Programme

validateProgrammeId :: String -> ValidationResult String
validateProgrammeId id
  | null id                       = Invalid ["Programme ID is empty"]
  | otherwise                     = Valid id

validateProgrammeName :: String -> ValidationResult String
validateProgrammeName name
  | null name                     = Invalid ["Programme name is empty"]
  | length name > 100             = Invalid ["Programme name must be 100 characters or less"]
  | otherwise                     = Valid name

validateProgramme :: Programme -> ValidationResult Programme
validateProgramme prog =
  case (validateProgrammeId (programmeId prog),
        validateProgrammeName (programmeName prog)) of
    (Valid _, Valid _) -> Valid prog
    (idResult, nameResult) ->
      let errors = concat [getErrors idResult "Programme ID",
                          getErrors nameResult "Programme Name"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidProgramme :: Programme -> Bool
isValidProgramme prog =
  case validateProgramme prog of
    Valid _ -> True
    Invalid _ -> False