------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_clock.html>
--
-- Try:
--
-- > > audition tick
--
module Pssd.Engineering.Clock where

import Data.List (foldl')

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | A cog. Base building block in this module.
--
-- > > audition $ out2 cog1
--
aCog :: UGen
aCog = c1 + c2 + c3
  where
    c1 = mkCog t n0 6666 4 30e-3
    c2 = mkCog t n0 7012 4 20e-3
    c3 = mkCog t n0 1500 3.5 10e-3
    t = ctrl "t_trig" 1
    n0 = whiteNoise 'n' ar

-- | Tick sound.
tick :: UGen
tick = out2 $ sig * ctrl "amp" 1
  where
    sig = foldl' (\a b -> bpf (delayL a 0.1 b) 420 0.3 * 0.3 + a) sig0
          [19e-3,11e-3,18e-3,23e-3,8e-3,13e-3,7e-3,3e-3]
    sig0 = select idx (mce sigs)
    sigs = [esc,tk1+esc,esc,tk2+esc]
    idx = pulseCount trg 0 `mod` (constant $ length sigs)
    trg = impulse kr 8 0
    tk1 = hpf (sum [c1,c2,c3]) 200 * 0.33
    tk2 = hpf (sum [c4,c5,c6]) 200 * 0.33
    esc = mkCog trg (sinOsc ar 10000 0) 8000 1 10e-3 +
          mkCog (tDelay trg 5e-3) (sinOsc ar 12000 0) 8000 1 10e-3 +
          mkCog (tDelay trg 12e-3) (sinOsc ar 9000 0) 8000 1 10e-3
    c1 = cog1 trg n
    c2 = cog2 (tDelay trg 3e-3) n
    c3 = cog3 (tDelay trg 7e-3) n
    c4 = cog4 trg n
    c5 = cog5 (tDelay trg 4e-3) n
    c6 = cog6 (tDelay trg 9e-3) n
    n = hpf (whiteNoise 'z' ar) 300

cog1 :: UGen -- ^ Trigger
     -> UGen -- ^ Noise
     -> UGen
cog1 t n = sum $ zipWith3 f [6666,7012,1500] [4,4,1] [30e-3,10e-3,10e-3]
  where f = mkCog t n

cog2 :: UGen -> UGen -> UGen
cog2 t n = sum $ zipWith3 f [4535,8325,11023] [4.5,4,4.6] [30e-3,25e-3,11e-3]
  where f = mkCog t n

cog3 :: UGen -> UGen -> UGen
cog3 t n = sum $ zipWith3 f [3267,4790,6470] [4,2,2] [40e-3,22e-3,8e-3]
  where f = mkCog t n

cog4 :: UGen -> UGen -> UGen
cog4 t n = sum $ zipWith3 f [2869,5190,6235] [4,2,2] [38e-3,18e-3,4e-3]
  where f = mkCog t n

cog5 :: UGen -> UGen -> UGen
cog5 t n = sum $ zipWith3 f [4489,7906,9940] [4.3,3.8,2.9] [30e-3,12e-3,9e-3]
  where f = mkCog t n

cog6 :: UGen -> UGen -> UGen
cog6 t n = sum $ zipWith3 f [6698,5890,2470] [4,4,1] [30e-3,12e-3,11e-3]
  where f = mkCog t n

-- | Make single click sound.
mkCog :: UGen -- ^ Trigger
      -> UGen -- ^ Noise
      -> UGen -- ^ Frequency
      -> UGen -- ^ Q, reciprocal
      -> UGen -- ^ Duration
      -> UGen
mkCog trg n freq q dur = bpf (n * decay2 trg 1e-3 dur) freq (1/q)
