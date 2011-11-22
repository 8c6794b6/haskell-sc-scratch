module Main where

import System.Environment (getArgs)
import Test.ReadWrite

{-
Using storable vector runs faster, depending on file size.
When reading and writing small files is the most concern, try to avoid using repa.
-}
main :: IO ()
main = do
  ifile:ofile:_ <- getArgs
  test_copy ifile ofile
  -- test_copy_vec ifile ofile
  -- test_copy_vec ifile ofile
  return ()
