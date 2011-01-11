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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_stream.html>
--
-- Try:
--
-- > > audition stream5
--
module Pssd.Earth.Stream where

import Sound.SC3
import Sound.SC3.ID

import Pssd.Util

-- | Stream, first try.
stream1 :: UGen
stream1 = out 0 $ mce2 sig sig
  where
    sig = sinOsc ar freq 0 * 5e-2
    freq = tRand 'f' 800 2400 fTrig
    fTrig = impulse kr (1/6e-3) 0

-- | Stream, second try.
stream2 :: UGen
stream2 = out 0 $ mce2 sig sig
  where
    sig = bpf (sinOsc ar freq 0) 1800 0.5 * 5e-2
    freq = lag (tRand 'f' 800 2400 fTrig) 5e-3
    fTrig = impulse kr (1/6e-3) 0

-- | Stream, third try.
--
-- Using @amplitude@ ugen to mimic @delta~@ in pd.
--
stream3 :: UGen
stream3 = out 0 $ mce2 sig sig
  where
    sig = bpf sig' 1800 0.5
    sig' = func trg 800 2400 5e-3 4e-2 + func trg 600 1800 8e-3 5e-2
    trg = impulse kr (1/3e-3) 0
    func tr lo hi df mul = oscil * mul * delta
      where
        oscil = sinOsc ar freq 0
        freq = lag (tRand 'a' lo hi tr) df
        delta = cubed (amplitude ar oscil 0.01 0.01)

-- | Stream, fourth try.
stream4 :: UGen
stream4 = out 0 $ mce2 sig sig
  where
    sig = hpf (sig0 + (sig1 * 6e-3) + (sig2 * 3e-3)) 800 * 40
    sig0 = hpf (bpf (sig1 * sig2) 1000 8) 1000 * 0.4
    sig1 = bpf sig1' 2000 0.5
    sig2 = bpf sig2' 1700 0.5
    sig1' = (cur1 * 4e-2) + (cur2 * 5e-2) + hpf (cur1 * cur2 * 0.2) 1000
    sig2' = (cur3 * 1e-1) + (cur4 * 1e-1) + hpf (cur3 * cur4 * 0.2) 1000
    cur1 = mkCurrency 'a' trg1 800 2400 11e-3
    cur2 = mkCurrency 'b' trg1 600 1800 11e-3
    cur3 = mkCurrency 'c' trg2 900 2700 11e-3
    cur4 = mkCurrency 'd' trg2 1100 2300 11e-3
    trg1 = impulse kr (1/3e-3) 0
    trg2 = impulse kr (1/5e-3) 0

-- | Helper for making currency noise.
mkCurrency :: (ID a)
           => a    -- ^ label for trigger
           -> UGen -- ^ trigger
           -> UGen -- ^ lo frequency
           -> UGen -- ^ high frequency
           -> UGen -- ^ delta for frequency change
           -> UGen
mkCurrency label trg lo hi df = oscil * delta
  where
    oscil = sinOsc ar freq 0
    freq = linExp (lfNoise1 'z' kr (1/df)) (-1) 1 lo hi
    delta = lpf (cubed $ amplitude ar oscil 1e-3 1e-3) 10

-- | Stream, fifth try.
--
-- It sounds more like mass of birds, rather than currency of water.
--
stream5 :: UGen
stream5 = out 0 $ mce2 sig sig
  where
    sig = lpf (hpf (bpf (sum curs) 2000 4e-1 * 500) 400) 500 * 20
    curs = [ cur12 * 0.05
           , cur34 * 0.09
           , hpf (bpf (cur12 * cur34) 700 8e-1) 1000 * 2 ]
    cur12 = hpf (bpf (cur1 * cur2 * 0.2) 1000 2e-1) 600
    cur34 = hpf (bpf (cur3 * cur4 * 0.2) 2000 2e-1) 700
    cur1 = mkCurrency 'a' trg1 800 2400 5e-3
    cur2 = mkCurrency 'b' trg1 600 1800 6e-3
    cur3 = mkCurrency 'c' trg2 900 2700 6e-3
    cur4 = mkCurrency 'd' trg2 1100 2300 8e-3
    trg1 = impulse kr (1/3e-3) 0
    trg2 = impulse kr (1/5e-3) 0
