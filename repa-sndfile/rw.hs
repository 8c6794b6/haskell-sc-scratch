module Main where

import System.Environment (getArgs)
import Test.ReadWrite

main :: IO ()
main = do
  ifile:ofile:_ <- getArgs
  -- copy_raw ifile ofile
  -- test_copy ifile ofile
  -- test_copy_vec ifile ofile
  test_rw ifile ofile
  return ()
