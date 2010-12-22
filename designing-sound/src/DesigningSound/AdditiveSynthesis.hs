------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Additive synthesis. Corresponding url for this module is:
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Additive_synthesis>
--
-- /Example/:
--
-- > > audition additive
--
-- And move mouse.
--
module DesigningSound.AdditiveSynthesis where

import Sound.SC3

additive :: UGen
additive = out 0 $ pan2 (son * 0.3) 0 1
  where
    son = num / denom
    num = sin theta - (idx * sin (theta - beta))
    denom = 1 + squared idx - (2 * idx * cos beta)
    theta = phasor ar 0 freq 0 (2 * pi) 0
    beta = phasor ar 0 (freq * distance) 0 (2 * pi) 0
    idx = mouseY kr 0.42 0.99 Linear 0.1
    distance = 3
    freq = mouseX kr 100 1000 Exponential 0.1 / sampleRate