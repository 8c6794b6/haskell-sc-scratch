{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Sound.SC3.Lepton.QuickCheck (Gen) where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

import Sound.OSC
import Sound.SC3

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

instance Arbitrary Datum where
  arbitrary = oneof
    [Int32 <$> arbitrary
    ,Int64 <$> arbitrary
    ,Float <$> arbitrary
    ,ASCII_String <$> arbitrary
    ,Blob . BL.pack . take 120 <$> listOf (elements [0..255])
    ,TimeStamp <$> arbitrary
    ,Midi <$> arbitrary]

instance Arbitrary MIDI where
    arbitrary =
        MIDI <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ASCII where
    arbitrary = B.pack <$> arbitrary

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
