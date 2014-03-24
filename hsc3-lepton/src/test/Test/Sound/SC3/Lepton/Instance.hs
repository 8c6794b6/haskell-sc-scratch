{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Test.Sound.SC3.Lepton.Instance where

import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Sound.OSC
import Sound.SC3.Lepton.Instance
import Sound.SC3.Lepton.QuickCheck

prop_arbitrary_Datum :: Datum -> Bool
prop_arbitrary_Datum n = n == n

tests :: TestTree
tests = $(testGroupGenerator)
