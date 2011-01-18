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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_acid.html>
--
-- Try:
--
-- > > withSC3 playAcid
--
module Pssd.Future.Acid where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | First acid sound, hissing.
acid1 :: UGen
acid1 = out2 sig
  where
    sig = clip2 sig1 1 * 2 * amp
    sig1 = bpf sig2 5000 (1/5)
    sig2 = sig3 * sig4 * 12
    sig3 = bpf sig5 2519 (1/69)
    sig4 = bpf sig5 4094 (1/69)
    sig5 = whiteNoise 'a' ar
    amp = ctrl "amp" 1

-- | Second acid sound, bubbly.
acid2 :: UGen
acid2 = out2 sig
  where
    sig = hpf (bpf (clip2 (sig1*8) 0.8) 3000 (1/5)) 4000 * amp
    sig1 = (sig2 * sinOsc ar 100 0) * sinOsc ar 500 0
    sig2 = sinOsc ar freq 0 * ampEnv
    ampEnv = decay2 t_trig 15e-3 10e-3 * 15
    freq = decay2 t_trig 40e-3 10e-3 * (freqBase * 5 + 30) * freqMul
    freqBase = tRand 't' 0 30 t_trig
    freqMul = freqBase * 80 + 6600
    t_trig = ctrl "t_trig" 1
    amp = ctrl "amp" 1

-- | Trigger for each bubble in acid2 sound.
acidTrig :: UGen
acidTrig = out outBus tr
  where
    outBus = ctrl "out" 100
    tr = dust 'd' kr freq
    freq = ctrl "freq" 1

-- | Play acid1 and acid2 sound with acidTrig.
playAcid :: (Transport t) => t -> IO ()
playAcid fd = do
  let dr (n,u) = async fd $ d_recv $ synthdef n u
  mapM_ dr [("acid1",acid1)
           ,("acid2",acid2)
           ,("acidTrig",acidTrig)]
  addNode 0 acidGraph fd

-- | Node graph for acid.
acidGraph :: SCNode
acidGraph =
  Group 0
    [Group 1
      [Group 16
        [Group 160
          [Synth 1600 "acidTrig"
            ["freq":=12,"out":=100]]
        ,Group 161
          [Synth 1610 "acid1"
            ["amp":=3]
          ,Synth 1611 "acid2"
            ["t_trig":<-100]]]]]