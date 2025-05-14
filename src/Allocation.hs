{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Allocation
  ( Allocation(..)
  , validateAllocations
  ) where

import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord(..), ToNamedRecord(..), (.:), (.=), namedRecord)
import Data.List (groupBy)
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lecturer (Lecturer(..))
import Module (Module(..))
import StudentGroup (StudentGroup(..))

-- | Represents an allocation of a lecturer to a module and group
--   sourced from the sample allocations CSV
data Allocation = Allocation
  { allocLecturer :: !String  -- ^ lecturerID foreign key
  , allocModule   :: !String  -- ^ courseID foreign key
  , allocGroup    :: !String  -- ^ studentGroup foreign key
  , allocHours    :: !Int     -- ^ hours assigned
  } deriving (Show, Generic)

instance FromNamedRecord Allocation where
  parseNamedRecord r = Allocation
    <$> r .: "lecturerID"
    <*> r .: "courseID"
    <*> r .: "studentGroup"
    <*> r .: "allocHours"

instance ToNamedRecord Allocation where
  toNamedRecord Allocation{..} = namedRecord
    [ "lecturerID"  .= allocLecturer
    , "courseID"    .= allocModule
    , "studentGroup".= allocGroup
    , "allocHours"  .= allocHours
    ]

-- | Validate allocations for foreign-key existence and over-allocation
validateAllocations
  :: [Lecturer]    -- ^ list of known lecturers
  -> [Module]      -- ^ list of known modules
  -> [StudentGroup]-- ^ list of known student groups
  -> [Allocation]  -- ^ allocations to validate
  -> [String]      -- ^ list of error messages
validateAllocations lecs mods groups allocs =
  let lecIDs = map lecturerID lecs
      modIDs = map courseID mods
      grpIDs = map groupID groups

      -- Foreign-key errors
      fkErrors = concat
        [ [ "Missing lecturer: " ++ a
          | Allocation a _ _ _ <- allocs, a `notElem` lecIDs
          ]
        , [ "Missing module: " ++ m
          | Allocation _ m _ _ <- allocs, m `notElem` modIDs
          ]
        , [ "Missing group: " ++ g
          | Allocation _ _ g _ <- allocs, g `notElem` grpIDs
          ]
        ]

      -- Sum allocated hours per lecturer
      totals :: Map String Int
      totals = Map.fromListWith (+)
        [ (allocLecturer a, allocHours a)
        | a <- allocs
        ]

      -- Over-allocation errors
      overAllocErrors =
        [ "Lecturer " ++ lecturerName lec
          ++ " allocated " ++ show used
          ++ " but only has " ++ show avail ++ " available"
        | lec@Lecturer{ lecturerID = lid, availableHours = avail } <- lecs
        , let used = Map.findWithDefault 0 lid totals
        , used > avail
        ]
  in fkErrors ++ overAllocErrors
