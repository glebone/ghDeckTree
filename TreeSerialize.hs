{-# LANGUAGE OverloadedStrings #-}

module TreeSerialize
  ( saveProgramState
  , loadProgramState
  ) where

import ProgramState (ProgramState)
import System.IO (withFile, IOMode(WriteMode, ReadMode), hPutStrLn, hGetContents)

-- | Save the ProgramState by writing 'show state' into a file.
saveProgramState :: FilePath -> ProgramState -> IO ()
saveProgramState path st =
  withFile path WriteMode $ \h -> do
    hPutStrLn h (show st)

-- | Load the ProgramState by reading the file contents and using 'read'.
loadProgramState :: FilePath -> IO ProgramState
loadProgramState path = do
  contents <- readFile path
  let st = read contents
  return st
