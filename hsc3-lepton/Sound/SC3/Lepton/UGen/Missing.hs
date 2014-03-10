------------------------------------------------------------------------------
-- | Some ugens missing in hsc3-0.13.
--

module Sound.SC3.Lepton.UGen.Missing where

import Sound.SC3

-- | Integrator filter.
integrator :: UGen -> UGen -> UGen
integrator i coef = mkFilter "Integrator" [i, coef] 1

fromDoneAction :: DoneAction -> UGen
fromDoneAction DoNothing = Constant_U (Constant 0)
fromDoneAction PauseSynth = Constant_U (Constant 1)
fromDoneAction RemoveSynth = Constant_U (Constant 2)
fromDoneAction (DoneAction u) = u
