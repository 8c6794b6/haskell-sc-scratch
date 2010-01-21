------------------------------------------------------------------------------
-- | Some ugens missing in hsc3-0.7.
-- 
-- Yes, Some ugens are missing. 
--

module Missing where

import Sound.SC3
import Sound.SC3.UGen.UGen

-- | Integrator filter.
integrator :: UGen -> UGen -> UGen
integrator i coef = mkFilter "Integrator" [i, coef] 1



