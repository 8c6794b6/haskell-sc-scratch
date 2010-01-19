------------------------------------------------------------------------------
-- | Common names for controls. Import with "qualified".
-- 

module Scratch.ControlArgs where

import Reusable

add = "add" @= 0
amp = "amp" @= 0.3
bufnum = "bufnum" @= 1
bus = "bus" @= 0
cut = "cut" @= 4000
cutoff = "cutoff" @= 10000
decay = "decay" @= 4
delay = "delay" @= 0.2
ffreq = "ffreq" @= 440
freq = "freq" @= 440
gain = "gain" @= 1
gate = "gate" @= 0
in' = "in" @= 0
i_bus = "i_bus" @= 10
i_start = "i_start" @= 1000
i_end = "i_end" @= 500
i_time = "i_time" @= 1
lfoBus = "lfobus" @= 0
mul = "mul" @= 0.1
out = "out" @= 0 
pan = "pan" @= 0
postGain = "postGain" @= 1
preGain = "preGain" @= 1
rate = "rate" @= 1
rez = "rez" @= 1
rezz = "rezz" @= 1
sustain = "sustain" @= 1
trig = "trig" @= 1