------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Sirens.
--
-- <http://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider/Sirens>
--
module DesigningSound.Sirens where

import Sound.SC3

import DesigningSound.Util

-- | Using lagUD and lfPulse for both pitch curve and waveform synthesis.
--
-- > SynthDef(\dsaf_horn1, { |rate=0.1|
-- >   var freq = LFPulse.kr(rate, 0.99, 0.4).lagud(0.4 / rate, 0.6 / rate) * 800 + 300;
-- >   var son  = LFPulse.ar(freq, 0.99, 0.2).lagud(0.4 / freq, 0.6 / freq) * 2 - 1;
-- >
-- >   // This filtering is a simple approximation of the plastic horn acoustics:
-- >   son = BPF.ar(son.clip2(0.2), 1500, 1/4) * 4;
-- >
-- >   // delay and reverb, to simulate the environment in which we hear the siren
-- >   son = son + DelayC.ar(son, 0.1, 0.1, 0.3);
-- >   son = son + FreeVerb.ar(son);
-- >
-- >   Out.ar(0, Pan2.ar(son * 0.4));
-- > }).memStore;
--
dsaf_horn1 :: UGen
dsaf_horn1 = out 0 $ pan2 (son * 0.4) 0 1
  where
    son = son' + freeVerb son' 0.5 0.5  0.5
    son' = son'' + delayC son'' 0.1 0.1 * 0.3
    son'' = bpf (clip2 son''' 0.2) 1500 (1/4) * 4
    son''' = lagUD (lfPulse ar freq 0.99 0.2) (0.4/freq) (0.6/freq) * 2 - 1
    freq = lagUD (lfPulse kr rate 0.99 0.4) (0.4/rate) (0.6/rate) * 800 + 300
    rate = control kr "rate" 0.1
