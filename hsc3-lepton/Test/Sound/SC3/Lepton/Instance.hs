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

import Sound.OSC
import Sound.SC3.Lepton.Instance
import Sound.SC3.Lepton.QuickCheck

tests :: [Property]
tests =
  [label "arbitrary_datum" prop_arbitrary_Datum
  ]

prop_arbitrary_Datum :: Datum -> Bool
prop_arbitrary_Datum n = n == n
