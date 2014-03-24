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
import qualified Test.Sound.SC3.Lepton.Parser as Parser
import qualified Test.Sound.SC3.Lepton.Tree.Diff as Diff
import qualified Test.Sound.SC3.Lepton.Tree.Nd as Nd
import qualified Test.Sound.SC3.Lepton.Tree.Tree as Tree
import qualified Test.Sound.SC3.Lepton.Tree.Zipper as Zipper
import qualified Test.Sound.SC3.Lepton.UGen.Demand as Demand
-- import qualified Test.Sound.SC3.Lepton.Pattern.ParseP as PP
-- import qualified Test.Sound.SC3.Lepton.Pattern.Interpreter as Interpreter
-- import qualified Test.Sound.SC3.Lepton.Pattern.Interpreter.R as PIR

main :: IO ()
main = do
  let tests = testGroup "tests"
        [ Instance.tests
        , Parser.tests
        , Diff.tests
        , Nd.tests
        , Tree.tests
        , Zipper.tests
        , Demand.tests
        ]
  defaultMain $ tests
