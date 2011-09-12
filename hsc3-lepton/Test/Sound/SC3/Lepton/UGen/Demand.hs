{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (TypeSynonymInstances)
--
module Test.Sound.SC3.Lepton.UGen.Demand where

import Control.Applicative
import System.Random
import Test.QuickCheck

import Data.Generics.Uniplate.Operations

import Sound.SC3
import Sound.SC3.Lepton.Instance ()
import Sound.SC3.Lepton.QuickCheck ()
import Sound.SC3.Lepton.UGen.Demand

instance Arbitrary UGen where
  arbitrary = constant <$> (arbitrary :: Gen Double)

instance Arbitrary Supply where
  arbitrary = oneof [sval <$> arbitrary]

tests :: [Property]
tests =
  [label "instance" prop_instance
  ,label "sbrown" prop_sbrown
  ,label "sbufrd" prop_sbufrd
  ,label "sbufwr" prop_sbufwr
  ,label "sgeom" prop_sgeom
  ,label "sibrown" prop_sibrown
  ,label "sinf" prop_sinf
  ,label "siwhite" prop_siwhite
  ,label "srand" prop_srand
  ,label "sseq" prop_sseq
  ,label "sser" prop_sser
  ,label "sseries" prop_sseries
  ,label "sstutter" prop_sstutter
  ,label "sswitch" prop_sswitch
  ,label "sswitch1" prop_sswitch1
  ,label "swhite" prop_swhite
  ,label "sxrand" prop_sxrand
  ]

prop_instance :: Property
prop_instance =
  forAll (arbitrary :: Gen Supply) $ \d ->
  d == d && show d == show d &&
  d + d == d * (return 2) &&
  d - d == 0

prop_sbrown :: Property
prop_sbrown =
  forAll arbitrary $ \(a,b,c,d) ->
  sbrown a b c d `hasUGenNamed` "Dbrown"

prop_sbufrd :: Property
prop_sbufrd =
  forAll arbitrary $ \(a,b,c) -> 
  sbufrd a b c `hasUGenNamed` "Dbufrd"

prop_sbufwr :: Property
prop_sbufwr =
  forAll arbitrary $ \(a,b,c,d) ->
  sbufwr a b c d `hasUGenNamed` "Dbufwr"

prop_sgeom :: Property
prop_sgeom =
  forAll arbitrary $ \(a,b,c) ->
  sgeom a b c `hasUGenNamed` "Dgeom"

prop_sibrown :: Property
prop_sibrown =
  forAll arbitrary $ \(a,b,c,d) ->
  sibrown a b c d `hasUGenNamed` "Dibrown"

prop_sinf :: Gen Bool
prop_sinf = do
  let p (Constant v) | v == 9.0e8 = True
      p _            = False
      d = evalSupply sinf (mkStdGen 0)
  return $ not $ null [e | e <- universe d, p e]

prop_siwhite :: Property
prop_siwhite =
  forAll arbitrary $ \(a,b,c) ->
  siwhite a b c `hasUGenNamed` "Diwhite"

prop_srand :: Property
prop_srand =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  srand a bs `hasUGenNamed` "Drand"

prop_sseq :: Property
prop_sseq =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sseq a bs `hasUGenNamed` "Dseq"

prop_sser :: Property
prop_sser =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sser a bs `hasUGenNamed` "Dser"

prop_sseries :: Property
prop_sseries =
  forAll arbitrary $ \(a,b,c) ->
  sseries a b c `hasUGenNamed` "Dseries"

prop_sstutter :: Property
prop_sstutter =
  forAll arbitrary $ \(a,b) ->
  sstutter a b `hasUGenNamed` "Dstutter"

prop_sswitch :: Property
prop_sswitch =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sswitch a bs `hasUGenNamed` "Dswitch"

prop_sswitch1 :: Property
prop_sswitch1 =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sswitch1 a bs `hasUGenNamed` "Dswitch1"

prop_swhite :: Property
prop_swhite =
  forAll arbitrary $ \(a,b,c) ->
  swhite a b c `hasUGenNamed` "Dwhite"

prop_sxrand :: Property
prop_sxrand =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sxrand a bs `hasUGenNamed` "Dxrand"

hasUGenNamed :: Supply -> String -> Gen Bool
hasUGenNamed d n = do
  let p (Primitive _ n' _ _ _ _) | n == n' = True
      p _                        = False
      d' = evalSupply d (mkStdGen 0)
  return $ not $ null [e | e <- universe d', p e]
