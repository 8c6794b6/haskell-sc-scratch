{-# OPTIONS_HADDOCK hide #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Arbitrary instance of SCNode and SynthParam for QuickCheck tests.
--
module Sound.SC3.Lepton.QuickCheck () where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

import Sound.OpenSoundControl
import Sound.SC3

import qualified Data.ByteString.Lazy as BL

import Sound.SC3.Lepton.Tree

instance Arbitrary SCNode where
  arbitrary = do
    l <- choose (0,3::Int)
    oneof [Group <$> arbitrary <*> vectorOf l arbitrary
          ,Synth <$> arbitrary <*> nameChars <*> arbitrary]
  -- arbitrary = sized nodes where
  --   nodes n | n < 2     = Synth <$> arbitrary <*> nameChars <*> arbitrary
  --           | otherwise = Group <$> arbitrary <*> Q.listOf (nodes (n `div` 2))

instance Arbitrary SynthParam where
  arbitrary = oneof
    [(:=)  <$> nameChars <*> arbitrary
    ,(:<-) <$> nameChars <*> (arbitrary `suchThat` (> 0))
    ,(:<=) <$> nameChars <*> arbitrary]

instance Arbitrary Datum where
  arbitrary = oneof
    [Int <$> arbitrary
    ,Float <$> arbitrary
    ,String <$> arbitrary
    ,Blob . BL.pack . take 120 <$> listOf (elements [0..255])
    ,TimeStamp <$> arbitrary
    ,Midi <$> ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)]

instance Arbitrary Time where
  arbitrary = oneof
    [UTCr <$> arbitrary
    ,NTPr <$> arbitrary
    ,NTPi <$> arbitrary]

instance Arbitrary AddAction where
  arbitrary = elements [AddToHead .. AddReplace]

instance Arbitrary Loop where
  arbitrary = oneof
    [return Loop, return NoLoop
    ,(WithLoop . constant) <$> (arbitrary::Gen Double)]

instance Arbitrary EnvCurve where
  arbitrary = oneof
    [elements [EnvStep, EnvLin, EnvExp, EnvSin, EnvCos
              ,EnvSqr, EnvCub]
    ,(EnvNum . constant) <$> (arbitrary::Gen Double)]

instance CoArbitrary SynthParam where
  coarbitrary (n:=v) = caSynthParam 0 n v
  coarbitrary (n:<-v) = caSynthParam 1 n v
  coarbitrary (n:<=v) = caSynthParam 2 n v

caSynthParam :: (CoArbitrary a1, CoArbitrary a2) => Int-> a1 -> a2 -> Gen a -> Gen a
caSynthParam i n v = variant i . coarbitrary n . coarbitrary v

instance CoArbitrary SCNode where
  coarbitrary (Synth i n ps) =
    variant (0::Int) . coarbitrary i . coarbitrary n . coarbitrary ps
  coarbitrary (Group i ns) =
    variant (1::Int) . coarbitrary i . coarbitrary ns

nameChars :: Gen String
nameChars = listOf1 (elements $ ['A' .. 'Z'] ++ ['a'..'z'] ++ "_.")

instance Arbitrary SCZipper where
  arbitrary = SCZipper <$> arbitrary <*> arbitrary

instance Arbitrary SCPath where
  arbitrary = SCPath <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary SCPath where
  coarbitrary (SCPath n ls rs) =
    variant (2::Int) . coarbitrary n . coarbitrary ls . coarbitrary rs

instance CoArbitrary SCZipper where
  coarbitrary (SCZipper n ps) =
    variant (3::Int) . coarbitrary n . coarbitrary ps

instance Arbitrary Step where
  arbitrary = oneof [return GoUp, return GoTop, GoDown <$> arbitrary]

instance CoArbitrary Step where
  coarbitrary s = case s of
    GoUp     -> variant (0::Int)
    GoTop    -> variant (1::Int)
    GoDown n -> variant (2::Int) . coarbitrary n
