------------------------------------------------------------------------------
-- | Common names for controls. Intended to use with @qualified@ import.
-- 

module Sound.SC3.Wing.ControlArg where

import Sound.SC3.Wing.Util

accent = "accent" @= 0
add = "add" @= 0
amp = "amp" @= 0.3
attack = "attack" @= 0.1
baseFreqBuf = "baseFreqBuf" @= 0
bufBase = "bufBase" @= 0
bufnum = "bufnum" @= 1
bus = "bus" @= 0
cut = "cut" @= 4000
cutoff = "cutoff" @= 10000
damp = "damp" @= 0
decay = "decay" @= 4
decayScale = "decayScale" @= 1
delay = "delay" @= 0.2
dur = "dur" @= 1
ffreq = "ffreq" @= 440
freq = "freq" @= 440
freqlag = "freqlag" @= 1
end = "end" @= 0
gain = "gain" @= 1
gate = "gate" @= 0
in' = "in" @= 0
inBus = "inBus" @= 0
i_bus = "i_bus" @= 10
i_end = "i_end" @= 500
i_out = "i_out" @= 0
i_start = "i_start" @= 1000
i_time = "i_time" @= 1
lfoBus = "lfobus" @= 0
mix = "mix" @= 0
mod = "mod" @= 1
mul = "mul" @= 0.1
mw = "mw" @= 0
out = "out" @= 0 
outBus = "outBus" @= 0
pan = "pan" @= 0
plfofreq = "plfofreq" @= 1
pitch = "pitch" @= 1
postGain = "postGain" @= 1
preamp = "preamp" @= 1
preGain = "preGain" @= 1
range = "range" @= 1
rate = "rate" @= 1
rez = "rez" @= 1
rezz = "rezz" @= 1
room = "room" @= 0
rq = "rq" @= 0
start = "start" @= 0
stretch = "stretch" @= 1
sustain = "sustain" @= 1
time = "time" @= 1
trig = "trig" @= 1
val = "val" @= 0