------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Entry points for tests ran by cabal.
--
module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (Args(..),quickCheckWithResult, stdArgs)

import Test.Sound.SC3.Lepton.Common
import qualified Test.Sound.SC3.Lepton.Instance as I
import qualified Test.Sound.SC3.Lepton.Parser as P
import qualified Test.Sound.SC3.Lepton.Tree.Diff as TD
import qualified Test.Sound.SC3.Lepton.Tree.Tree as TT
import qualified Test.Sound.SC3.Lepton.Tree.Zipper as TZ
import qualified Test.Sound.SC3.Lepton.UGen.Demand as UD
import qualified Test.Sound.SC3.Lepton.Pattern as PT

main :: IO ()
main = do
  results <- mapM (quickCheckWithResult stdArgs {maxSize=25}) $ concat
    [ P.tests, TT.tests, I.tests, TZ.tests , TD.tests
    , UD.tests, PT.tests]
  if any (not . isSuccess) results then exitFailure else exitSuccess
