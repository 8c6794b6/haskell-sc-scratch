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

import Test.Tasty (testGroup, defaultMain)

import Test.Sound.SC3.Lepton.Common
import qualified Test.Sound.SC3.Lepton.Instance as Instance
-- import qualified Test.Sound.SC3.Lepton.Parser as P
-- import qualified Test.Sound.SC3.Lepton.Tree.Diff as TD
import qualified Test.Sound.SC3.Lepton.Tree.Nd as Nd
import qualified Test.Sound.SC3.Lepton.Tree.Tree as Tree
-- import qualified Test.Sound.SC3.Lepton.Tree.Zipper as TZ
import qualified Test.Sound.SC3.Lepton.UGen.Demand as Demand
-- import qualified Test.Sound.SC3.Lepton.Pattern.ParseP as PP
-- import qualified Test.Sound.SC3.Lepton.Pattern.Interpreter as PI
-- import qualified Test.Sound.SC3.Lepton.Pattern.Interpreter.R as PIR

main :: IO ()
main = do
  let tests = testGroup "tests"
        [ Instance.tests
        , Nd.tests
        , Demand.tests
        , Tree.tests
        ]
  defaultMain $ tests
