------------------------------------------------------------------------------
-- | Scratchy UGens.

module Scratch.UGen where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Wing
import Sound.OpenSoundControl

-- | Simple percussive pitched synth.
simplePitched :: UGen
simplePitched = out 0 $ mce [sig, sig]
    where
      sig = lpf ((sinOsc ar f 0 + saw ar f) * e * a) 3000
      f = control kr "freq" 440
      e = envGen kr 1 1 0 (220 / f) RemoveSynth $ shape
      a = control kr "amp" 0.2
      shape = env [0, 1, 0.8, 0.5, 0] [0.01, 0.2, 0.01, 0.01] [EnvCub] 1 1

-- | Simple gated envelope pitched synth.
simpleGated :: UGen
simpleGated = out 0 $ mce [sig, sig]
    where
      sig = o * e * a
      o = sinOsc ar (mce [f, f*1.998, f*2.001, f*3.002, f*3.998]) 0
      e = envGen kr g 1 0 1 RemoveSynth $ shape
      f = control kr "freq" 440
      a = control kr "amp" 0.05
      g = control kr "gate" 1
      shape = env [0, 1, 0.8, 0] [0.01, 0.1, 0.1] [EnvNum (-4)] 1 0

-- | Gated organ-ish synthdef.
gateOrgan01 :: UGen
gateOrgan01 = out 0 $ mce [sig, sig]
    where
      sig = lpf o 4000 * eAmp

      -- sum of fm oscillators
      o = sum $ map (\n -> mkFM (f*n)) [1,2,4,8]
      mkFM freq = sinOsc ar (freq+mod) 0
          where mod = sinOsc ar (freq*0.5) 0 * freq * 0.5

      -- gated amplitude envelope
      eAmp = envGen kr g a 0 1 RemoveSynth $
             env [0, 1, 0.8, 0] [0.05, 0.1, 0.1] [EnvNum (-5), EnvNum (-4)] 1 0
                      
      -- controls
      f = control kr "freq" 440
      a = control kr "amp" 0.05
      g = control kr "gate" 1

-- | Simple gendy1 synthdef.
simpleGendy1 :: UGen
simpleGendy1 = out o $ pan2 sig pos 1 where
    sig = gendy1 ar ampDist durDist adParam ddParam minFreq maxFreq
          ampScale durScale initCPs kNum
    o = control kr "out" 0 
    ampDist = control kr "ampDist" 1
    durDist = control kr "durDist" 1
    adParam = control kr "adParam" 1 
    ddParam = control kr "ddParam" 1
    minFreq = control kr "minFreq" 440
    maxFreq = control kr "maxFreq" 880
    ampScale = control kr "ampScale" 0.5
    durScale = control kr "durScale" 0.5
    initCPs = control kr "initCPs" 12
    kNum = control kr "kNum" 12
    pos = control kr "pos" 0