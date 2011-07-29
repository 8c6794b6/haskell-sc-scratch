-- MyUGens.hs
module MyUGens where

import Sound.SC3
import Sound.SC3.Lepton

ug001 :: UGen
ug001 = 
  out 0 $ sinOsc ar ("freq"@@440) 0 * 
  envGen kr 1 1 0 1 RemoveSynth (envPerc 1e-2 1)

ug002 :: UGen
ug002 = 
  out 0 $ lfPar ar 330 0 * 
  envGen kr 1 1 0 1 RemoveSynth (envPerc 5e-2 5e-2)
