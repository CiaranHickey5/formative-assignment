-- Module.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Module
  ( Module(..)
  , validateModuleName
  , validateModuleHours
  , validateModule
  , isValidModule
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv

data Module = Module
  { moduleName   :: !String
  , moduleHours  :: !Int
  } deriving (Show, Generic)

instance FromNamedRecord Module where
  parseNamedRecord r = Module
    <$> r .: "module-name"
    <*> r .: "module-hours"

instance ToNamedRecord Module

validateModuleName :: String -> ValidationResult String
validateModuleName name
  | null name                     = Invalid ["Module name is empty"]
  | length name > 50              = Invalid ["Module name must be 50 characters or less"]
  | otherwise                     = Valid name

validateModuleHours :: Int -> ValidationResult Int
validateModuleHours hours
  | hours <= 0                    = Invalid ["Module hours must be positive"]
  | hours > 12                    = Invalid ["Module hours cannot exceed 12 per week"]
  | otherwise                     = Valid hours

validateModule :: Module -> ValidationResult Module
validateModule module' =
  case (validateModuleName (moduleName module'),
        validateModuleHours (moduleHours module')) of
    (Valid _, Valid _) -> Valid module'
    (nameResult, hoursResult) ->
      let errors = concat [getErrors nameResult "Module Name",
                          getErrors hoursResult "Module Hours"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidModule :: Module -> Bool
isValidModule module' =
  case validateModule module' of
    Valid _ -> True
    Invalid _ -> False