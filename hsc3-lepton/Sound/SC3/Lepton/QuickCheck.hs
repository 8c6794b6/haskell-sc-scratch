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
  (Arbitrary(..), Gen, choose, vectorOf, oneof, listOf1, elements, sized)
import qualified Test.QuickCheck as Q

import Sound.OpenSoundControl

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
    [(:=) <$> nameChars <*> arbitrary
    ,(:<-) <$> nameChars <*> arbitrary
    ,(:<=) <$> nameChars <*> arbitrary]

instance Arbitrary Datum where
  arbitrary = oneof
    [Int <$> arbitrary
    ,Float <$> arbitrary
    ,String <$> arbitrary
    ,Blob <$> arbitrary
    ,TimeStamp <$> arbitrary
    ,Midi <$> ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)]

instance Arbitrary Time where
  arbitrary = oneof
    [UTCr <$> arbitrary
    ,NTPr <$> arbitrary
    ,NTPi <$> arbitrary]

nameChars :: Gen String
nameChars = listOf1 (elements $ ['A' .. 'Z'] ++ ['a'..'z'] ++ "_.")
