------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Pedestrian sounds. Corresponding url for this module is:
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Pedestrians>
--

module DesigningSound.Pedestrians where

import Sound.SC3

pedestrian :: UGen
pedestrian = out 0 $ sig * pls
  where
    sig = sinOsc ar 2500 0 * 0.2
    pls = lfPulse ar 5 0 0.5