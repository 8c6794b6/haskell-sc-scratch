module Main where

import System.Environment (getArgs)
import Test.ReadWrite

{-
Using storable vector runs faster, depending on file size.
-}
main :: IO ()
main = do
  ifile:ofile:_ <- getArgs
  test_copy ifile ofile
  -- test_copy_vec ifile ofile
  -- test_copy_vec ifile ofile
  return ()
