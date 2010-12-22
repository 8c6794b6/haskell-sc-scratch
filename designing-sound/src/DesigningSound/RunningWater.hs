------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Running water
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Running_water>
--
-- /Example/:
--
-- > audition randomFreq
-- > audition combinedRising
--
module DesigningSound.RunningWater where

import Sound.SC3
import Sound.SC3.ID

-- | Random frequency sines
--
-- > x = {SinOsc.ar(LFNoise0.kr(170).range(800, 2400), 0, 0.3)}.play;
--
randomFreq :: UGen
randomFreq = out 0 $ sinOsc ar freq 0 * 0.3
  where freq = linLin (lfNoise0 'a' kr 170 ) (-1) 1 800 2400

-- | Rising pitches.
--
-- > x = {
-- >   var trigs, freq;
-- >   trigs = Dust.kr(170);
-- >   freq =
-- >      // Generally choose from a varied base freq
-- >      TExpRand.kr(800, 2000, trigs)
-- >      // Wobbly variation
-- >      + LFNoise2.kr(20, mul: 300)
-- >      // General tendency for upward rise
-- >      + EnvGen.kr(Env.perc(1).range(0,17), trigs)
-- >      ;
-- >   SinOsc.ar(freq, 0, 0.3)
-- > }.play;
-- >
risingPitches :: UGen
risingPitches = out 0 $ mkRising 'a' 'b' 'c'

-- | Helper for making risingPitch
mkRising :: (ID a) => a -> a -> a -> UGen
mkRising a b c = sinOsc ar freq 0 * 0.3
  where
    freq = (tExpRand a 800 2000 trigs) +
           (lfNoise2 b kr 20 * 300) +
           (envGen kr trigs 1 0 1 DoNothing (envPerc 1 1) * 17)
    trigs = dust c kr 170

-- | Combining few risingPitches parallel.
--
-- Extra band pass and high pass filter is added in haskell implementation.
--
-- > x = {
-- >    var trigs, freq;
-- >    6.collect{
-- >       trigs = Dust.kr(170);
-- >       freq =
-- >          // Generally choose from a varied base freq
-- >          TExpRand.kr(800, 2000, trigs)
-- >          // Wobbly variation
-- >          + LFNoise2.kr(20, mul: 300)
-- >          // General tendency for upward rise
-- >          + EnvGen.kr(Env.perc(1).range(0,17), trigs)
-- >          ;
-- >       SinOsc.ar(freq, 0, 0.3)
-- >    }.mean
-- > }.play;
combinedRising :: UGen
combinedRising = out 0 (hpf (bpf (sum rs / 6) 700 8) 800 * 0.1)
  where rs = take 6 $ zipWith3 mkRising [(101::Int)..] [201..] [301..]