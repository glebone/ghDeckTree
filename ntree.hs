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
import Data.Time
  ( getCurrentTime
  , getCurrentTimeZone
  , utcToLocalTime
  , toGregorian
  , fromGregorian
  , UTCTime(..)
  , diffUTCTime
  , formatTime
  , defaultTimeLocale
  )
import Data.Maybe (listToMaybe)
import Data.List (elemIndex)

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
-- A bank of 50 possible "toy" symbols. Press 'd' to add one to an edge.
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
-- The tree lines, but the top row (index 0) will be replaced dynamically by
-- whichever star is currently chosen.
--------------------------------------------------------------------------------
treeTemplate :: [String]
treeTemplate =
  [ "         *         "  -- Will get replaced by dynamic star
  , "        ***        "
  , "       *****       "
  , "      *******      "
  , "     *********     "
  , "    ***********    "
  , "         o         "
  , "        o o        "
  , "       o   o       "
  , "      o     o      "
  , "     o       o     "
  , "    o         o    "
  , "   o           o   "
  , "  o  o  o  o  o  o "
  , "         ###       "
  ]

--------------------------------------------------------------------------------
-- Let's define a list of "edges" in the tree (row,col) so that toys can appear
-- on the leftmost or rightmost non-space character in each row.
--------------------------------------------------------------------------------
treeEdges :: [(Int, Int)]
treeEdges = collectEdges treeTemplate

collectEdges :: [String] -> [(Int, Int)]
collectEdges linesOfTree =
  concatMap findEdges (zip [0..] linesOfTree)
  where
    findEdges (row, line) =
      let trimmed = dropWhile (==' ') line
          len     = length line
          leftIdx  = len - length trimmed
          rightIdx = len - 1 - length (dropWhile (==' ') (reverse line))
      in if leftIdx <= rightIdx
         then [(row, leftIdx) | not (all (==' ') line)]  -- left edge
           ++ [(row, rightIdx) | rightIdx > leftIdx]      -- right edge
         else []

--------------------------------------------------------------------------------
-- Main entry: set up non-blocking input, then run the loop with initial state.
--------------------------------------------------------------------------------
main :: IO ()
main = do
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
--   1) Draw everything (tree + countdown + local time + usage + debug)
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
--   1) Choose a random symbol from 'toyBank'
--   2) Choose a random edge from 'treeEdges'
--   3) Append to the toy list
--   4) Update debugMsg so we can see which symbol & position was chosen
--------------------------------------------------------------------------------
addRandomToy :: ProgramState -> IO ProgramState
addRandomToy st
  | null toyBank || null treeEdges =
      return st { debugMsg = "No toy added (empty toyBank or treeEdges)." }
  | otherwise = do
      -- Pick random symbol
      sym <- randomElem toyBank
      -- Pick random edge
      (r, c) <- randomElem treeEdges
      let newToy   = (r, c, sym)
      let newToys  = toys st ++ [newToy]
      let dbg      = "Placing symbol '" ++ [sym] ++
                     "' at (" ++ show r ++ "," ++ show c ++ ")"
      return st { toys = newToys, debugMsg = dbg }

randomElem :: [a] -> IO a
randomElem [] = error "Cannot select from an empty list."
randomElem xs = do
  i <- randomRIO (0, length(xs) - 1)
  return (xs !! i)

--------------------------------------------------------------------------------
-- drawAll: draws the blinking tree, the countdown, the local time, usage, & debug
--------------------------------------------------------------------------------
drawAll :: ProgramState -> IO ()
drawAll st = do
    -- Build a list of strings (the tree) with the correct star & toys
    let topper   = topSymbols !! starIndex st
    let treeRows = buildTree topper (toys st)

    -- Draw the tree
    forM_ (zip [0..] treeRows) $ \(row, line) -> do
      setCursorPosition row 0
      putStr line

    -- Then draw the countdown and current time
    (countdownStr, timeStr) <- newYearInfo
    let countdownRow = length treeRows + 1
    setCursorPosition countdownRow 0
    putStrLn countdownStr

    let timeRow = countdownRow + 1
    setCursorPosition timeRow 0
    putStrLn timeStr

    -- Then draw usage instructions
    let usageRow = timeRow + 2
    setCursorPosition usageRow 0
    putStrLn "Press 'n' to change the star topper, 'd' to add a random toy."
    setCursorPosition (usageRow + 1) 0
    putStrLn "Press Ctrl+C to exit."

    -- Finally, draw the debug message
    let debugRow = usageRow + 2
    setCursorPosition debugRow 0
    putStrLn ("Debug: " ++ debugMsg st)

--------------------------------------------------------------------------------
-- buildTree:
--   1) Safely check that treeTemplate is not empty.
--   2) Replace the top line's star with 'topper'
--   3) For each 'o', color it green.
--   4) Place any toys (row, col, symbol).
--   5) Return the resulting list of strings.
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

    -- Convert 'o' to green ANSI-coded 'o'
    finalRows      = map colorLights withToys
  in finalRows
  where
    replaceStar :: String -> String -> String
    replaceStar line starSym =
      case elemIndex '*' line of
        Just idx -> take idx line ++ starSym ++ drop (idx + 1) line
        Nothing  -> line

    placeToy :: [String] -> (Int, Int, Char) -> [String]
    placeToy rows (r, c, sym)
      | r < 0 || r >= length rows       = rows
      | c < 0 || c >= length (rows !! r) = rows
      | otherwise =
          let oldLine = rows !! r
              newLine = take c oldLine ++ [sym] ++ drop (c + 1) oldLine
          in take r rows ++ [newLine] ++ drop (r + 1) rows

    colorLights :: String -> String
    colorLights = concatMap (\ch ->
      if ch == 'o'
        then "\x1b[32mo\x1b[0m"  -- green 'o'
        else [ch]
      )

--------------------------------------------------------------------------------
-- newYearInfo: show how many seconds remain until New Year, plus local time
--------------------------------------------------------------------------------
newYearInfo :: IO (String, String)
newYearInfo = do
    now <- getCurrentTime
    tz  <- getCurrentTimeZone
    let localTime = utcToLocalTime tz now
    let timeStr   = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localTime

    -- Figure out how many seconds until Jan 1 of next year
    let (year, _, _)   = toGregorian (utctDay now)
        nextYear        = fromGregorian (year + 1) 1 1
        newYearMidnight = UTCTime nextYear 0
        secsUntil       = floor $ diffUTCTime newYearMidnight now

    let countdownStr = "Seconds until New Year: " ++ show secsUntil
    return (countdownStr, "Local time: " ++ timeStr)
