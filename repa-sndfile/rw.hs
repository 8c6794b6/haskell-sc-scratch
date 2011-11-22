module Main where

import System.Environment (getArgs)
import Data.Array.Repa.IO.Sndfile

main :: IO ()
main = do
  -- (i,a) <- test_read ifile
  -- print a
  
  ifile:ofile:_ <- getArgs
  -- print c
  -- copy_raw ifile ofile
  test_copy ifile ofile
  -- test_copy_vec ifile ofile
  return ()
  
