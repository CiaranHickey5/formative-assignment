{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
 


module Lecturer  where
import Types (ValidationResult(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isAlpha)
import qualified Data.Text as T
import qualified Data.ByteString as BL





data Lecturer = Lecturer
  { lecturerID   :: !String
  , name          :: !String
  , email         :: !String
  , departmentID  :: !String
  , availableHours      :: !Int
  } deriving (Show, Generic)


instance Csv.FromNamedRecord Lecturer where
  parseNamedRecord  r = Lecturer

    <$> r .: "lecturerID" 
    <*> r .: "name"
    <*> r .: "email"
    <*> r .: "departmentID"
    <*> r .: "availableHours"
    

instance ToNamedRecord  Lecturer

validateId :: String -> ValidationResult String
validateId id
  | length id /= 4                 = Invalid ["Lecturer ID must be 4 characters"]
  | not (isAlpha (head id))        = Invalid ["First character of Lecturer ID must be alphabetic"]
  | not (all isDigit (tail id))    = Invalid ["Last 3 characters of Lecturer ID must be digits"]
  | otherwise                      = Valid id

validateName :: String -> ValidationResult String
validateName name
  | null name                      = Invalid ["Name is empty"]
  | length name > 15               = Invalid ["Name must be 15 characters or less"]
  | otherwise                      = Valid name

validateEmail :: String -> ValidationResult String
validateEmail email
  | null email                     = Invalid ["Email is empty"]
  | '@' `notElem` email            = Invalid ["Email must contain '@'"]
  | '.' `notElem` dropWhile (/= '@') email = Invalid ["Email must contain '.' after '@'"]
  | otherwise                      = Valid email