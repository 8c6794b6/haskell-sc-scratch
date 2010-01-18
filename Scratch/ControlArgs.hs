------------------------------------------------------------------------------
-- | Common names for controls. Import with "qualified".
-- 

module Scratch.ControlArgs where

import Reusable

out = "out" @= 0 
in' = "in" @= 0
bus = "bus" @= 0
lfoBus = "lfobus" @= 0
freq = "freq" @= 440
ffreq = "ffreq" @= 440
add = "add" @= 0
mul = "mul" @= 0.1
gain = "gain" @= 1
preGain = "preGain" @= 1
postGain = "postGain" @= 1
pan = "pan" @= 0