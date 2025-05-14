{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Allocation
  ( Allocation(..)
  , validateAllocation
  , validateAllocations
  , isValidAllocation
  ) where

import Types (ValidationResult(..))
import Lecturer (Lecturer(..))
import Module (Module(..))
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord, (.:))
import qualified Data.Csv as Csv
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.List (find)

data Allocation = Allocation
  { lecturerName   :: !String
  , moduleName     :: !String
  , allocatedHours :: !Int
  , groupId        :: !String
  } deriving (Show, Generic)

instance FromNamedRecord Allocation where
  parseNamedRecord r = Allocation
    <$> r .: "lecturer-name"
    <*> r .: "module-name"
    <*> r .: "module-hours"
    <*> r .: "group-id"

instance ToNamedRecord Allocation

validateAllocation :: [Lecturer] -> [Module] -> Allocation -> ValidationResult Allocation
validateAllocation lecturers modules alloc =
  -- Validate that referenced entities exist
  case (findLecturer (lecturerName alloc) lecturers,
        findModule (moduleName alloc) modules) of
    (Nothing, _) -> Invalid ["Referenced lecturer not found: " ++ lecturerName alloc]
    (_, Nothing) -> Invalid ["Referenced module not found: " ++ moduleName alloc]
    (Just _, Just _) -> Valid alloc
  where
    findLecturer name' = find (\l -> name l == name')
    findModule name' = find (\m -> moduleName m == name')

-- Validate a list of allocations against lecturer availability
validateAllocations :: [Lecturer] -> [Module] -> [Allocation] -> [(Allocation, [String])]
validateAllocations lecturers modules allocations =
  -- Group allocations by lecturer
  let 
    lectGroups = groupBy ((==) `on` lecturerName) (sortOn lecturerName allocations)
    
    -- Validate each group
    validatedGroups = map (validateLecturerAllocations lecturers) lectGroups
    
    -- Flatten results
    result = concat validatedGroups
  in
    result
  where
    validateLecturerAllocations lectrs allocGrp@(firstAlloc:_) = 
      let 
        lectName = lecturerName firstAlloc
        totalHours = sum (map allocatedHours allocGrp)
        lecturer = find (\l -> name l == lectName) lectrs
      in
        case lecturer of
          Nothing -> map (\a -> (a, ["Lecturer not found: " ++ lectName])) allocGrp
          Just lect ->
            if totalHours > availableHours lect
              then map (\a -> (a, ["Lecturer " ++ lectName ++ " is allocated " ++ 
                                 show totalHours ++ " hours but only has " ++ 
                                 show (availableHours lect) ++ " available"])) allocGrp
              else map (\a -> (a, [])) allocGrp
    validateLecturerAllocations _ [] = []

isValidAllocation :: [Lecturer] -> [Module] -> Allocation -> Bool
isValidAllocation lecturers modules alloc =
  case validateAllocation lecturers modules alloc of
    Valid _ -> True
    Invalid _ -> False