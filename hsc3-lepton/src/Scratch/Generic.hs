{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Updating control param in ugen, generic approach.

We have a synth:

> synth001 = 
>   let sig = sinOsc AR ("freq"@@440) 0 * amp * e
>       amp = "amp"@@0.3
>       e = decay2 t 1e-2 dcy * l
>       t = impulse KR 1 0
>       l = line KR 1 0 8 RemoveSynth
>       dcy = "decay"@@2
>   in  out ("out"@@0) (mce2 sig sig)

We want to update default values in control, like:

> synth002 = setug "freq" 880 synth001

We want to update values multiple times:

> synth003 = setug "decay" 0.3 synth002

-}
module Scratch.Generic where

import Data.Generics.Uniplate.Data
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton.Instance
import Sound.SC3.Lepton.UGen

-- | Update control value of given UGen.
setc ::
  String    -- ^ Name of control parameter
  -> Double -- ^ New value
  -> UGen   -- ^ Target ugen
  -> UGen
setc name value = transform f where
  f (Control r name' _ t) | name == name' = Control r name value t
  f x = x
  
-- | Update control values of given UGen
setcs :: 
  [(String,Double)] -- ^ List of params to updat
  -> UGen -- ^ Target ugen
  -> UGen
setcs kvs ug = foldr (\(k,v) acc -> setc k v acc) ug kvs
  
synth001 :: UGen
synth001 = 
  let sig = sinOsc AR ("freq"@@440) 0 * amp * e
      amp = "amp"@@0.3
      e = decay2 t 1e-2 dcy * l
      t = impulse KR dense 0 + dust 'α' KR dense
      l = line KR 1 0 8 RemoveSynth
      dcy = "decay"@@2
      dense = "dense"@@1
  in  out ("out"@@0) (mce2 sig sig)

synth002 :: UGen
synth002 =
  setc "freq" 880 .
  setc "decay" 0.1 .
  setc "dense" 4 $ synth001
