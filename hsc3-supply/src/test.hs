{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import System.Random
import Sound.SC3

import Test.QuickCheck
import Test.Tasty (defaultMain)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Sound.SC3.Supply

main :: IO ()
main = defaultMain $(testGroupGenerator)

instance Arbitrary UGen where
  arbitrary = constant <$> (arbitrary :: Gen Double)

instance Arbitrary Loop where
  arbitrary = oneof $ map return [Loop,NoLoop]

instance Arbitrary Supply where
  arbitrary = oneof [sval <$> arbitrary]

prop_sbrown :: Property
prop_sbrown =
  forAll arbitrary $ \(a,b,c,d) ->
  sbrown a b c d `isPrimitiveUGen` "Dbrown"

prop_sbufrd :: Property
prop_sbufrd =
  forAll arbitrary $ \(a,b,c) ->
  sbufrd a b c `isPrimitiveUGen` "Dbufrd"

prop_sbufwr :: Property
prop_sbufwr =
  forAll arbitrary $ \(a,b,c,d) ->
  sbufwr a b c d `isPrimitiveUGen` "Dbufwr"

prop_sgeom :: Property
prop_sgeom =
  forAll arbitrary $ \(a,b,c) ->
  sgeom a b c `isPrimitiveUGen` "Dgeom"

prop_sibrown :: Property
prop_sibrown =
  forAll arbitrary $ \(a,b,c,d) ->
  sibrown a b c d `isPrimitiveUGen` "Dibrown"

prop_sinf :: Gen Bool
prop_sinf = do
  let p (Constant_U (Constant v)) | v == 9.0e8 = True
      p _                         = False
      d = evalSupply sinf (mkStdGen 0)
  return $ p d

prop_siwhite :: Property
prop_siwhite =
  forAll arbitrary $ \(a,b,c) ->
  siwhite a b c `isPrimitiveUGen` "Diwhite"

prop_srand :: Property
prop_srand =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  srand a bs `isPrimitiveUGen` "Drand"

prop_sseq :: Property
prop_sseq =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sseq a bs `isPrimitiveUGen` "Dseq"

prop_sser :: Property
prop_sser =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sser a bs `isPrimitiveUGen` "Dser"

prop_sseries :: Property
prop_sseries =
  forAll arbitrary $ \(a,b,c) ->
  sseries a b c `isPrimitiveUGen` "Dseries"

prop_sshuf :: Property
prop_sshuf =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sshuf a bs `isPrimitiveUGen` "Dshuf"

prop_sstutter :: Property
prop_sstutter =
  forAll arbitrary $ \(a,b) ->
  sstutter a b `isPrimitiveUGen` "Dstutter"

prop_sswitch :: Property
prop_sswitch =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sswitch a bs `isPrimitiveUGen` "Dswitch"

prop_sswitch1 :: Property
prop_sswitch1 =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sswitch1 a bs `isPrimitiveUGen` "Dswitch1"

prop_swhite :: Property
prop_swhite =
  forAll arbitrary $ \(a,b,c) ->
  swhite a b c `isPrimitiveUGen` "Dwhite"

prop_swrand :: Property
prop_swrand =
  forAll (arbitrary `suchThat` ((\bs -> length bs >= 2) . snd)) $ \(a,bs) ->
  swrand a bs bs `isPrimitiveUGen` "Dxrand"

prop_sxrand :: Property
prop_sxrand =
  forAll (arbitrary `suchThat` (not . null . snd)) $ \(a,bs) ->
  sxrand a bs `isPrimitiveUGen` "Dxrand"

isPrimitiveUGen :: Supply -> String -> Gen Bool
isPrimitiveUGen d n =
    return $ case evalSupply d (mkStdGen 0) of
        Primitive_U (Primitive _ n' _ _ _ _) -> n == n'
        _                                    -> False
