module Types where

import Data.Char (isDigit)

data ValidationResult a
  = Valid a
  | Invalid [String]
  deriving (Show)
-- this can be used throughout the project to validate data
