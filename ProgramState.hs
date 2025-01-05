{-# LANGUAGE OverloadedStrings #-}

module ProgramState
  ( ProgramState(..)
  ) where

-- | Data structure to hold the dynamic program state:
--   1) Which star index is currently at the top
--   2) A list of placed Toys (row, col, Char)
--   3) A debug message string
data ProgramState = ProgramState
  { starIndex :: Int
  , toys      :: [(Int, Int, Char)] -- ^ (row, column, symbol)
  , debugMsg  :: String
  }
  deriving (Show, Read) -- Deriving Read for loading from file
