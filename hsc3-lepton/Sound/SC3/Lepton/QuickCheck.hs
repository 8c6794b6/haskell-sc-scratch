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
  (Arbitrary(..), Gen, choose, oneof, vectorOf, listOf1, elements)
import qualified Test.QuickCheck as Q

import Sound.OpenSoundControl

import Sound.SC3.Lepton.Tree
import Sound.SC3.Lepton.Parser

instance Arbitrary SCNode where
  arbitrary = do
    l <- choose (0,4::Int)
    oneof [Group <$> arbitrary <*> vectorOf l arbitrary
          ,Synth <$> arbitrary <*> nameChars <*> vectorOf l arbitrary]

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
nameChars = listOf1 (elements $ ['A' .. 'Z'] ++ ['a'..'z'] ++ ['_', '.'])
