-- MyUGens.hs
module MyUGens where

import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

ug001 :: UGen
ug001 =
  out 0 $ sinOsc ar ("freq"@@440) 0 *
  envGen kr 1 1 0 1 RemoveSynth (envPerc 1e-2 1)

ug002 :: UGen -> UGen -> UGen -> UGen
ug002 t_trig amp freq =
  out 0 $ lfPar ar freq 0 *
  envGen kr t_trig amp 0 1 RemoveSynth (envPerc 5e-2 5e-2)
