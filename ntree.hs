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
  , fromGregorian -- Fixed here
  , UTCTime(..)
  , diffUTCTime
  , formatTime
  , defaultTimeLocale
  )

--------------------------------------------------------------------------------
-- Data structure to hold our dynamic program state:
--  1) Which star index is currently at top
--  2) A list of placed Toys (row, col, Char)
--------------------------------------------------------------------------------
data ProgramState = ProgramState
  { starIndex :: Int
  , toys      :: [(Int, Int, Char)] -- row, col, symbol
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
  [ '⚽','⚾','🥎','🏀','🏐','🏈','🏉','🎾','🎱','🏓'
  , '🏸','🥏','🪀','🏹','⛸','🪁','✈','🚀','🛸','🏆'
  , '🎧','🎵','♬','♩','♭','♯','🎶','🎷','🎸','🥁'
  , '🍎','🍊','🍇','🍒','🍓','🍉','🍀','🌸','🌹','🌻'
  , '🎃','🎄','💎','💖','⭐','💫','🎁','❄','⚡','🔥'
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
          leftIdx  = length line - length trimmed
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

    let initialState = ProgramState { starIndex = 0, toys = [] }
    loop initialState

--------------------------------------------------------------------------------
-- The main loop: each iteration:
--   1) Draw everything (tree + countdown + local time + usage)
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
            'n' -> return state { starIndex = (starIndex state + 1) `mod` length topSymbols }
            'd' -> addRandomToy state
            _   -> return state
        else return state

    -- 4) Clear and repeat
    clearScreen
    loop newState

--------------------------------------------------------------------------------
-- addRandomToy:
--   1) Choose a random symbol from 'toyBank'
--   2) Choose a random edge from 'treeEdges'
--   3) Append to the toy list
--------------------------------------------------------------------------------
addRandomToy :: ProgramState -> IO ProgramState
addRandomToy st = do
    -- Pick random symbol
    sym <- randomElem toyBank
    -- Pick random edge
    (r, c) <- randomElem treeEdges
    let newToy = (r, c, sym)
    -- Append to the toy list
    let newToys = toys st ++ [newToy]
    return st { toys = newToys }

randomElem :: [a] -> IO a
randomElem xs = do
  i <- randomRIO (0, length(xs) - 1)
  return (xs !! i)

--------------------------------------------------------------------------------
-- drawAll: draws the blinking tree, the countdown, the local time, and usage
--------------------------------------------------------------------------------
drawAll :: ProgramState -> IO ()
drawAll st = do
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

--------------------------------------------------------------------------------
-- buildTree:
--   1) Replace the top line's star with 'topper'
--   2) For each 'o', create a blinking colored light
--   3) For each toy coordinate, place the toy symbol
--      (toys override the 'o' or space at that position)
--   4) Return a list of "rendered" strings
--------------------------------------------------------------------------------
buildTree :: String -> [(Int,Int,Char)] -> [String]
buildTree topper placedToys =
    [ buildLine row (maybeTemplateLine row) | row <- [0..(length treeTemplate - 1)] ]
  where
    buildLine :: Int -> String -> String
    buildLine row lineTemplate =
      concatMap (renderChar row) (zip [0..] lineTemplate)

    renderChar :: Int -> (Int, Char) -> String
    renderChar row0 (col, ch)
      -- If there's a toy here, use it instead
      | Just toySym <- lookupToy row0 col = [toySym]
      | ch == 'o'  = blinkingLight  -- color/light effect
      | otherwise  = [ch]

    maybeTemplateLine :: Int -> String
    maybeTemplateLine 0 =
      -- Overwrite the first row's star
      replaceStar (head treeTemplate) topper
    maybeTemplateLine r =
      treeTemplate !! r

    -- If the original line has an asterisk at position p, replace that with 'topper'.
    replaceStar :: String -> String -> String
    replaceStar line topSym =
      case break (=='*') line of
        (before, '*' : after) -> before ++ topSym ++ after
        _                     -> line  -- fallback

    lookupToy :: Int -> Int -> Maybe Char
    lookupToy r c =
      case [sym | (rr, cc, sym) <- placedToys, rr == r, cc == c] of
        (x:_) -> Just x
        _     -> Nothing

--------------------------------------------------------------------------------
-- blinkingLight: returns an ANSI-escaped "o" with random color and blink
-- (We do the random color outside of buildTree to keep it fresh every call)
--------------------------------------------------------------------------------
blinkingLight :: String
blinkingLight =
  -- We'll pick the color each time we encounter 'o'
  -- for a bit of "variation" in each frame
  let colors = ["\x1b[32m", "\x1b[33m", "\x1b[31m"] -- green, yellow, red
      ansiBlink = "\x1b[5m"  -- may not be supported everywhere
      reset     = "\x1b[0m"
  in ansiBlink ++ pickColor colors ++ "o" ++ reset
  where
    -- For each draw, pick a random color index.
    -- A cheap hack: use the system clock (no pure RNG in a pure function).
    -- This is just to demonstrate. For a truly stable color, we'd do IO.
    pickColor :: [String] -> String
    pickColor cs =
      let seedFromClock = 0  -- We'll keep it static in pure code
      in cs !! (seedFromClock `mod` length cs)

--------------------------------------------------------------------------------
-- newYearInfo: returns (countdownString, localTimeString)
--------------------------------------------------------------------------------
newYearInfo :: IO (String, String)
newYearInfo = do
    now  <- getCurrentTime
    zone <- getCurrentTimeZone
    let localNow = utcToLocalTime zone now

    -- Calculate the upcoming New Year (YYYY+1, Jan 1, 00:00:00)
    let (y,m,d) = toGregorian (toEnum $ fromEnum (utctDay now))
        nextNY  = fromGregorian (y+1) 1 1
        nextNYUTC = UTCTime nextNY 0
        diffSec = realToFrac (diffUTCTime nextNYUTC now) :: Double

    let countdownStr = formatCountdown diffSec
    let timeStr      = "Current time: " ++
                       formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localNow

    return (countdownStr, timeStr)

--------------------------------------------------------------------------------
-- formatCountdown: given seconds to next NY, produce a string like:
--    "Time to New Year: 31 days 12h 05m 34s"
--------------------------------------------------------------------------------
formatCountdown :: Double -> String
formatCountdown secs
  | secs <= 0  = "Happy New Year!"
  | otherwise  =
      let totalSec  = floor secs :: Int
          days      = totalSec `div` 86400
          leftover  = totalSec `mod` 86400
          hours     = leftover `div` 3600
          leftover2 = leftover `mod` 3600
          minutes   = leftover2 `div` 60
          seconds   = leftover2 `mod` 60
      in  "Time to New Year: "
          ++ show days ++ "d "
          ++ show hours ++ "h "
          ++ show minutes ++ "m "
          ++ show seconds ++ "s"

