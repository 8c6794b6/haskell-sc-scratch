------------------------------------------------------------------------------
-- | Scratchy UGens.

module Scratch.UGen where

import Sound.SC3 
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
      shape = env [0, 1, 0.8, 0.2, 0] [0.05, 0.1, 0.05, 0.05] 
              [EnvNum (-4)] 1 0

