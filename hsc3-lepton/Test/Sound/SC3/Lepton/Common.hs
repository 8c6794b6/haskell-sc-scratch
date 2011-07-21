------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Common utility for tests.
--
module Test.Sound.SC3.Lepton.Common where

import Test.QuickCheck

import Sound.SC3.Lepton

isSuccess :: Result -> Bool
isSuccess r = case r of Success _ _ _ -> True; _ -> False

isGroup :: SCNode -> Bool
isGroup x = case x of Group _ _ -> True; _ -> False

isSynth :: SCNode -> Bool
isSynth n = case n of Synth _ _ _ -> True; _ -> False;

