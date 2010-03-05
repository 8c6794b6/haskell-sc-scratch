------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook03
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook03_External_Control/.
--
--

module SCHelp.PG.Cookbook03 where

import Control.Applicative
import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import SCQuery
import SCTree
import SCSched

main :: IO ()
main = undefined

-- $controlByMIDIOrHID
-- 
-- Translation of @Phidkey@ and @PhidSlot@ pattern in sclang.
-- 

-- $triggeringByGUI
-- 
-- Using GTK.
--

-- $triggeringBySignalAmplitude
-- 
-- A bit trickier, in sclang.
-- 

