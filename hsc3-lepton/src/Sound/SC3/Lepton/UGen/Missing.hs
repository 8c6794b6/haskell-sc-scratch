------------------------------------------------------------------------------
-- | Some ugens missing in hsc3-0.7.
-- 

module Sound.SC3.Lepton.UGen.Missing where

import Sound.SC3

-- | Integrator filter.
integrator :: UGen -> UGen -> UGen
integrator i coef = mkFilter "Integrator" [i, coef] 1

-- | In sc help file, @DetectSilence@ ugen has one output, but in
-- Sound.SC3.UGen.Filters, @detectSilence@ ugen was defined with no
-- output. This ugen is redefined with having single output.
detectSilence' :: UGen -> UGen -> UGen -> DoneAction -> UGen
detectSilence' i a t act = 
    mkFilter "DetectSilence" [i, a, t, from_done_action act] 1

-- | Limiter.
limiter :: UGen -> UGen -> UGen -> UGen
limiter i l d = mkFilter "Limiter" [i, l, d] 1

from_done_action :: DoneAction -> UGen
from_done_action DoNothing = Constant 0
from_done_action PauseSynth = Constant 1
from_done_action RemoveSynth = Constant 2
from_done_action (DoneAction u) = u

