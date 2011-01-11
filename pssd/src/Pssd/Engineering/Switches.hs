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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_switches.html>
--
module Pssd.Engineering.Switches where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Switch 1.
switch1 :: UGen
switch1 = out2 $ sig * ctrl "amp" 1
  where
    sig = n1 + n2 + n3
    n1 = mkN n0 trg 6666 40 1e-4 2e-3
    n2 = mkN n0 (tDelay trg 1e-3) 7012 40 1e-4 2e-3
    n3 = mkN n0 (tDelay trg 4e-3) 3500 35 1e-4 1e-3
    n0 = hpf (whiteNoise 'a' ar) 500
    trg = ctrl "t_trig" 1

-- | Switch, take 2, with damping.
switch2 :: UGen
switch2 = out2 $ sig * ctrl "amp" 1
  where
    sig = foldr (\a b -> bpf (delayL b 0.3 a) 2500 0.7 * 0.2 + b) sig0
          (map (*0.5) [83e-3,49e-3,37e-3,17e-3,23e-3,64e-3,13e-3,29e-3])
    sig0 = sum [s1,s2,s3,s4,s5,s6,s7,s8,s9]
    s1 = mkN n1 trg 5432 1.2 1e-3 2e-3
    s2 = mkN n1 (delayN trg 1 8e-3) 2200 1.2 1e-3 20e-3
    s3 = mkN n1 (delayN trg 1 16e-3) 3212 1.2 1e-3 10e-3
    s4 = delayN s1 1 35e-3
    s5 = delayN s2 1 38e-3
    s6 = delayN s3 1 41e-3
    s7 = mkN n2 (delayN trg 1 70e-3) 6666 4 1e-3 20e-3
    s8 = mkN n2 (delayN trg 1 82e-3) 7012 4 1e-3 20e-3
    s9 = mkN n2 (delayN trg 1 78e-3) 3500 3.5 1e-3 10e-3
    n1 = hpf (whiteNoise 'a' ar) 500
    n2 = hpf (whiteNoise 'b' ar) 500
    trg = ctrl "t_trig" 1

-- | Single click of switch.
mkN :: UGen -- ^ Noise
    -> UGen -- ^ Trigger
    -> UGen -- ^ Frequency
    -> UGen -- ^ Reciprocal of Q
    -> UGen -- ^ Attack time
    -> UGen -- ^ Release time
    -> UGen
mkN n t freq q atk rel = bpf n freq (1/q) * decay2 t atk rel
