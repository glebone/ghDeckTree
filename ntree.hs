{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
  ( hSetBuffering
  , hSetEcho
  , hWaitForInput
  , stdin
  , stdout
  , BufferMode(NoBuffering)
  )
import System.Console.ANSI
  ( clearScreen
  , setCursorPosition
  , hideCursor
  , showCursor
  )
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (elemIndex, elemIndices)
import Debug.Trace (trace)


--------------------------------------------------------------------------------
-- Data structure to hold our dynamic program state:
--  1) Which star index is currently at top
--  2) A list of placed Toys (row, col, Char)
--  3) A debug message string (to show what's happening)
--------------------------------------------------------------------------------
data ProgramState = ProgramState
  { starIndex  :: Int
  , toys       :: [(Int, Int, Char)] -- row, col, symbol
  , debugMsg   :: String
  } deriving (Show)

--------------------------------------------------------------------------------
-- A list of possible tree toppers. Press 'n' to cycle.
--------------------------------------------------------------------------------
topSymbols :: [String]
topSymbols =
  [ "*"   -- ASCII asterisk (default)
  , "★"   -- Black Star (U+2605)
  , "🌟"  -- Glowing Star (U+1F31F)
  , "✨"  -- Sparkles (U+2728)
  , "☆"  -- White Star (U+2606)
  , "❇"  -- Sparkle (U+2747)
  ]

--------------------------------------------------------------------------------
-- A bank of 50 possible "toy" symbols. Press 'd' to add one to the tree.
--------------------------------------------------------------------------------
toyBank :: [Char]
toyBank =
  [ '⚽','⚾','🏟','🏀','🏐','🏈','🏉','🎾','🎱'
  , '🏍','⛸','🚁','🚀','🛸','🏆','🎵','♬','♩'
  , '♭','♯','🎶','🎷','🎸','🧠','🍎','🍊','🍇'
  , '🍒','🍓','🍉','🌈','🌸','🌺','🌻','🌱'
  , '🎄','❄','✨','☆','★','♨','🔥'
  ]

--------------------------------------------------------------------------------
-- The updated tree lines with two triangles.
--------------------------------------------------------------------------------
treeTemplate :: [String]
treeTemplate =
  [ "         *         "  -- Top star
  , "        ***        "  -- Top triangle
  , "       *****       "
  , "      *******      "
  , "     *********     "
  , "    ***********    "  -- End of top triangle
  , "   *************   "  -- Bottom triangle
  , "  ***************  "
  , " ***************** "
  , "         *         "  -- Second top star
  , "        ***        "  -- Second top triangle
  , "       *****       "
  , "      *******      "
  , "     *********     "
  , "    ***********    "  -- End of second top triangle
  , "   *************   "  -- Second bottom triangle
  , "  ***************  "
  , " ***************** "
  , "*******************"
  , "*********************"
  , "         ###       "  -- Tree trunk
  ]

--------------------------------------------------------------------------------
-- Let's define a list of all `*` positions in the tree where toys can appear.
--------------------------------------------------------------------------------
treeStars :: [(Int, Int)]
treeStars = collectStars treeTemplate

collectStars :: [String] -> [(Int, Int)]
collectStars linesOfTree =
  concatMap findStars (zip [0..] linesOfTree)
  where
    findStars (row, line) = [(row, col) | col <- elemIndices '*' line]

--------------------------------------------------------------------------------
-- buildTree:
--   1) Replace the top line's star with 'topper'
--   2) Place any toys at specific positions
--   3) Return a list of "rendered" strings
--------------------------------------------------------------------------------
buildTree :: String -> [(Int, Int, Char)] -> [String]
buildTree _     _        | null treeTemplate = []
buildTree topper placedT =
  let
    -- We know treeTemplate has at least one line:
    (topLine:restOfTree) = treeTemplate

    -- Replace the star in the first line
    replacedTopRow = replaceStar topLine topper
    baseRows       = replacedTopRow : restOfTree

    -- Place each toy in 'placedT'
    withToys       = foldl placeToy baseRows placedT
  in withToys
  where
    replaceStar :: String -> String -> String
    replaceStar line starSym =
      case elemIndex '*' line of
        Just idx -> take idx line ++ starSym ++ drop (idx + 1) line
        Nothing  -> line

    placeToy :: [String] -> (Int, Int, Char) -> [String]
    placeToy rows (r, c, sym)
      | r < 0 || r >= length rows = rows
      | c < 0 || c >= length (rows !! r) = rows
      | otherwise =
          let oldLine = rows !! r
              newLine = take c oldLine ++ [sym] ++ drop (c + 1) oldLine
          in take r rows ++ [newLine] ++ drop (r + 1) rows

--------------------------------------------------------------------------------
-- Main entry: set up non-blocking input, then run the loop with initial state.
--------------------------------------------------------------------------------
main :: IO ()
main = do
    -- Debug: Print treeStars
    putStrLn "Debug: Available star positions:"
    print treeStars

    -- Turn off buffering and echo so we can read single chars without blocking
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering

    hideCursor
    clearScreen

    let initialState = ProgramState
          { starIndex = 0
          , toys      = []
          , debugMsg  = ""
          }
    loop initialState

--------------------------------------------------------------------------------
-- The main loop: each iteration:
--   1) Draw everything (tree + usage + debug)
--   2) Wait ~ half a second
--   3) Check if a key was pressed:
--        'n' -> next star
--        'd' -> place a random toy
--      otherwise no change
--   4) Clear screen, repeat
--------------------------------------------------------------------------------
loop :: ProgramState -> IO ()
loop state = do
    -- 1) Draw
    drawAll state

    -- 2) Wait half a second
    threadDelay 500000

    -- 3) Check if a key was pressed
    keyPressed <- hWaitForInput stdin 0
    newState <-
      if keyPressed
        then do
          c <- getChar
          case c of
            'n' -> return state
              { starIndex = (starIndex state + 1) `mod` length topSymbols
              , debugMsg  = "Switched to next star."
              }
            'd' -> addRandomToy state
            _   -> return state { debugMsg = "No action for key: " ++ show c }
        else return state

    -- 4) Clear and repeat
    clearScreen
    loop newState

--------------------------------------------------------------------------------
-- addRandomToy:
--   1) Choose a random position from `treeStars`
--   2) Choose a random symbol from `toyBank`
--   3) Append to the toy list
--------------------------------------------------------------------------------
addRandomToy :: ProgramState -> IO ProgramState
addRandomToy st
  | null toyBank = return st { debugMsg = "No toy added (toyBank is empty)." }
  | null treeStars = return st { debugMsg = "No toy added (no stars available)." }
  | otherwise = do
      -- Pick a random position
      (r, c) <- randomElem treeStars
      -- Pick a random toy
      sym <- randomElem toyBank
      let newToy   = (r, c, sym)
      let newToys  = toys st ++ [newToy]
      let dbg      = "Placing symbol '" ++ [sym] ++ "' at (" ++ show r ++ "," ++ show c ++ ")"
      return st { toys = newToys, debugMsg = dbg }

randomElem :: [a] -> IO a
randomElem [] = error "Cannot select from an empty list."
randomElem xs = do
  i <- randomRIO (0, length(xs) - 1)
  return (xs !! i)

--------------------------------------------------------------------------------
-- drawAll: draws the tree and usage instructions
--------------------------------------------------------------------------------
drawAll :: ProgramState -> IO ()
drawAll st = do
    let topper   = topSymbols !! starIndex st
    let treeRows = buildTree topper (toys st)

    -- Draw the tree
    forM_ (zip [0..] treeRows) $ \(row, line) -> do
      setCursorPosition row 0
      putStr line

    -- Draw debug message
    let debugRow = length treeRows + 1
    setCursorPosition debugRow 0
    putStrLn ("Debug: " ++ debugMsg st)
