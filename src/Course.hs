{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
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

import           GHC.Generics              (Generic)
import           Data.Char                 (isAlphaNum)
import           Data.Csv                  ( FromNamedRecord(..)
                                           , ToNamedRecord(..)
                                           , DefaultOrdered(..)
                                           , (.=)
                                           , (.:)
                                           , namedRecord
                                           , header
                                           )
import           Types                     (ValidationResult(..))

--------------------------------------------------------------------------------
-- * Course type
--------------------------------------------------------------------------------

data Course = Course
  { courseID     :: !String    -- ^ e.g. "C103"
  , courseName   :: !String    -- ^ e.g. "Functional Programming"
  , credits      :: !Int       -- ^ number of credits / contact hours
  , departmentID :: !String    -- ^ e.g. "CompSci"
  } deriving (Show, Generic)

instance FromNamedRecord Course where
  parseNamedRecord r = Course
    <$> r .: "courseID"
    <*> r .: "name"
    <*> r .: "credits"
    <*> r .: "departmentID"

instance ToNamedRecord Course where
  toNamedRecord Course{..} =
    namedRecord
      [ "courseID"     .= courseID
      , "name"         .= courseName
      , "credits"      .= credits
      , "departmentID" .= departmentID
      ]

instance DefaultOrdered Course where
  -- explicitly spell out the header row
  headerOrder _ = header ["courseID","name","credits","departmentID"]

--------------------------------------------------------------------------------
-- * Validation
--------------------------------------------------------------------------------

validateCourseId :: String -> ValidationResult String
validateCourseId cid
  | null cid                 = Invalid ["Course ID is empty"]
  | length cid < 3           = Invalid ["Course ID must be at least 3 characters"]
  | head cid /= 'C'          = Invalid ["Course ID must start with 'C'"]
  | not (all isAlphaNum cid) = Invalid ["Course ID must be alphanumeric"]
  | otherwise                = Valid cid

validateCourseName :: String -> ValidationResult String
validateCourseName nm
  | null nm           = Invalid ["Course name is empty"]
  | length nm > 50    = Invalid ["Course name must be ≤ 50 characters"]
  | otherwise         = Valid nm

validateCredits :: Int -> ValidationResult Int
validateCredits c
  | c < 0     = Invalid ["Credits cannot be negative"]
  | c > 60    = Invalid ["Credits cannot exceed 60"]
  | otherwise = Valid c

validateCourseDepartmentId :: String -> ValidationResult String
validateCourseDepartmentId did
  | null did  = Invalid ["Department ID is empty"]
  | otherwise = Valid did

validateCourse :: Course -> ValidationResult Course
validateCourse course =
  case ( validateCourseId     (courseID course)
       , validateCourseName   (courseName course)
       , validateCredits      (credits course)
       , validateCourseDepartmentId (departmentID course)
       ) of
    (Valid _, Valid _, Valid _, Valid _) -> Valid course
    (idR, nmR, crR, dpR) ->
      let errs = concat
            [ prefix "ID" idR
            , prefix "Name" nmR
            , prefix "Credits" crR
            , prefix "Department" dpR
            ]
      in Invalid errs
  where
    prefix :: String -> ValidationResult a -> [String]
    prefix p (Invalid es) = map (\e -> p ++ ": " ++ e) es
    prefix _ (Valid _)    = []

isValidCourse :: Course -> Bool
isValidCourse c = case validateCourse c of
  Valid _   -> True
  Invalid _ -> False
