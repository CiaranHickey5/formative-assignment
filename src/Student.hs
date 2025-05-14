{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Student
  ( Student(..)
  , validateStudentId
  , validateStudentName
  , validateStudentEmail
  , validateProgramId
  , validateStudent
  , isValidStudent
  ) where

import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)

data Student = Student
  { studentID  :: !String
  , studentName :: !String
  , studentEmail :: !String
  , programID  :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Student where
  parseNamedRecord r = Student
    <$> r .: "studentID"
    <*> r .: "name"
    <*> r .: "email"
    <*> r .: "programID"

instance ToNamedRecord Student

validateStudentId :: String -> ValidationResult String
validateStudentId id
  | length id /= 8                 = Invalid ["Student ID must be 8 characters"]
  | not (all isDigit id)           = Invalid ["Student ID must contain only digits"]
  | otherwise                      = Valid id

validateStudentName :: String -> ValidationResult String
validateStudentName name
  | null name                      = Invalid ["Name is empty"]
  | length name > 50               = Invalid ["Name must be 50 characters or less"]
  | otherwise                      = Valid name

validateStudentEmail :: String -> ValidationResult String
validateStudentEmail email
  | null email                     = Invalid ["Email is empty"]
  | '@' `notElem` email            = Invalid ["Email must contain '@'"]
  | '.' `notElem` dropWhile (/= '@') email = Invalid ["Email must contain '.' after '@'"]
  | not ("setu.ie" `isSuffixOf` email) = Invalid ["Email must end with 'setu.ie'"]
  | otherwise                      = Valid email
  where
    isSuffixOf suffix str = reverse suffix `isPrefixOf` reverse str
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

validateProgramId :: String -> ValidationResult String
validateProgramId programId
  | null programId                 = Invalid ["Program ID is empty"]
  | otherwise                      = Valid programId

validateStudent :: Student -> ValidationResult Student
validateStudent student =
  case (validateStudentId (studentID student),
        validateStudentName (studentName student),
        validateStudentEmail (studentEmail student),
        validateProgramId (programID student)) of
    (Valid _, Valid _, Valid _, Valid _) -> Valid student
    (idResult, nameResult, emailResult, programResult) ->
      let errors = concat [getErrors idResult "Student ID",
                          getErrors nameResult "Name",
                          getErrors emailResult "Email",
                          getErrors programResult "Program ID"]
      in Invalid errors
  where
    getErrors (Invalid errs) prefix = map (\err -> prefix ++ ": " ++ err) errs
    getErrors (Valid _) _ = []

isValidStudent :: Student -> Bool
isValidStudent student =
  case validateStudent student of
    Valid _ -> True
    Invalid _ -> False