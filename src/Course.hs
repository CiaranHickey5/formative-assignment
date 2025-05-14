{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Course
  ( Course(..)
  , validateCourseId
  , validateCourseName
  , validateCredits
  , validateCourseDepartmentId
  , validateCourse
  , isValidCourse
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.Char (isAlphaNum)

data Course = Course
  { courseID     :: !String
  , courseName   :: !String
  , credits      :: !Int
  , departmentID :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Course where
  parseNamedRecord r = Course
    <$> r .: "courseID"
    <*> r .: "name"
    <*> r .: "credits" 
    <*> r .: "departmentID"

instance ToNamedRecord Course

validateCourseId :: String -> ValidationResult String
validateCourseId id
  | null id                        = Invalid ["Course ID is empty"]
  | length id < 3                  = Invalid ["Course ID must be at least 3 characters"]
  | not (head id == 'C')           = Invalid ["Course ID must start with 'C'"]
  | not (all isAlphaNum id)        = Invalid ["Course ID must contain only alphanumeric characters"]
  | otherwise                      = Valid id

validateCourseName :: String -> ValidationResult String
validateCourseName name
  | null name                      = Invalid ["Course name is empty"]
  | length name > 50               = Invalid ["Course name must be 50 characters or less"]
  | otherwise                      = Valid name

validateCredits :: Int -> ValidationResult Int
validateCredits cred
  | cred < 0                       = Invalid ["Credits cannot be negative"]
  | cred > 60                      = Invalid ["Credits cannot exceed 60"]
  | otherwise                      = Valid cred

validateCourseDepartmentId :: String -> ValidationResult String
validateCourseDepartmentId departmentId
  | null departmentId              = Invalid ["Department ID is empty"]
  | otherwise                      = Valid departmentId

validateCourse :: Course -> ValidationResult Course
validateCourse course =
  case (validateCourseId (courseID course),
        validateCourseName (courseName course),
        validateCredits (credits course),
        validateCourseDepartmentId (departmentID course)) of
    (Valid _, Valid _, Valid _, Valid _) -> Valid course
    (idResult, nameResult, creditsResult, deptResult) ->
      let errors = concat [getErrors idResult "ID",
                          getErrors nameResult "Name",
                          getErrors creditsResult "Credits",
                          getErrors deptResult "Department"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidCourse :: Course -> Bool
isValidCourse course =
  case validateCourse course of
    Valid _ -> True
    Invalid _ -> False