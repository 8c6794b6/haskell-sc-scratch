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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_roar.html>
--
-- Try:
--
-- > > audition roar
--
module Pssd.Monster.Roar where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Using frac UnaryOp for wrap~ atom in pd.
ripple :: UGen -- ^ Input
       -> UGen
ripple input = cos (sqrt (frac (squared input)))

-- | Tract filter
comber :: UGen -- ^ Input signal
       -> UGen -- ^ Base
       -> UGen -- ^ Distance
       -> UGen -- ^ Resonance
       -> UGen
comber input base dis res = sig
  where
    sig = foldr f (bpf input base res) [1..8]
    f a b = bpf input (base + inc ^ a) res + b
    inc = sqrt 2

-- | Roar.
roar :: UGen
roar = out2 sig
  where
    sig = comber (ripple sig1) base dis res * ampEnv * amp
    sig1 = lfSaw ar (baseLine * 40) 0 * (baseLine * 0.5 + 1.385)
    base = (baseLine * 300) + 10
    dis = (baseLine * 20) + 100
    res = (baseLine * 7) + 60
    ampEnv = sqrt baseLine
    baseLine = (sin (lin * pi) + 1) / 2
    lin = envGen kr tr 1 0 1 DoNothing $
          env [0,0,0.5,1.5] [0,2,1.5] [EnvLin] (-1) (-1)
    tr = ctrl "t_trig" 1
    amp = ctrl "amp" 0.5
