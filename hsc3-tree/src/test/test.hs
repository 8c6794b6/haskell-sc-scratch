{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Entry point for cabal test suite.

-}
module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Sound.SC3.Tree.Diff as Diff
import qualified Test.Sound.SC3.Tree.Nd as Nd
import qualified Test.Sound.SC3.Tree.Type as Type
import qualified Test.Sound.SC3.Tree.Query as Query
import qualified Test.Sound.SC3.Tree.Zipper as Zipper

main :: IO ()
main = defaultMain $ testGroup "test"
  [ Diff.tests
  , Nd.tests
  , Type.tests
  , Query.tests
  , Zipper.tests
  ]
