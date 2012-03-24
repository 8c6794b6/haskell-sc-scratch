{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Main module for running test.

-}
module Main where

import Self.Dep
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  let xs = "foo is bar"
      xs' = fooSize xs
  if xs' == 10 then exitSuccess else exitFailure