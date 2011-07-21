------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Test.Sound.SC3.Lepton.Parser where

import Test.QuickCheck

import Sound.OpenSoundControl
import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.QuickCheck

tests :: [Property]
tests = [label "parse_datum" prop_parse_datum
        ,label "parse_any_datum" prop_parse_any_datum]

prop_parse_datum :: Property
prop_parse_datum =
  forAll (arbitrary `suchThat` (not . null)) $ \ds ->
  either (const False) (const True) (parse datum ds)

prop_parse_any_datum :: Property
prop_parse_any_datum =
  forAll (arbitrary `suchThat` (not . null)) $ \ds ->
  let isRight x = case x of Right _ -> True; _ -> False
  in (isRight $ parse int ds) .||.
     (isRight $ parse float ds) .||.
     (isRight $ parse double ds) .||.
     (isRight $ parse string ds) .||.
     (isRight $ parse blob ds) .||.
     (isRight $ parse timeStamp ds) .||.
     (isRight $ parse midi ds)