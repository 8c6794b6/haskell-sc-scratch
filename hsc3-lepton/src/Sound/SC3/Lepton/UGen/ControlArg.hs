------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Common names for controls. Intended to use with @qualified@ import.
--
module Sound.SC3.Lepton.UGen.ControlArg where

import Sound.SC3 (UGen)

import Sound.SC3.Lepton.Util

accent :: UGen
accent = "accent" @= 0

add :: UGen
add = "add" @= 0

amp :: UGen
amp = "amp" @= 0.3

attack :: UGen
attack = "attack" @= 0.1

baseFreqBuf :: UGen
baseFreqBuf = "baseFreqBuf" @= 0

bufBase :: UGen
bufBase = "bufBase" @= 0

bufnum :: UGen
bufnum = "bufnum" @= 1

bus :: UGen
bus = "bus" @= 0

cut :: UGen
cut = "cut" @= 4000

cutoff :: UGen
cutoff = "cutoff" @= 10000

damp :: UGen
damp = "damp" @= 0

decay :: UGen
decay = "decay" @= 4

decayScale :: UGen
decayScale = "decayScale" @= 1

delay :: UGen
delay = "delay" @= 0.2

dur :: UGen
dur = "dur" @= 1

end :: UGen
end = "end" @= 0

ffreq :: UGen
ffreq = "ffreq" @= 440

freq :: UGen
freq = "freq" @= 440

freqlag :: UGen
freqlag = "freqlag" @= 1

gain :: UGen
gain = "gain" @= 1

gate :: UGen
gate = "gate" @= 0

i_bus :: UGen
i_bus = "i_bus" @= 10

i_end :: UGen
i_end = "i_end" @= 500

i_out :: UGen
i_out = "i_out" @= 0

i_start :: UGen
i_start = "i_start" @= 1000

i_time :: UGen
i_time = "i_time" @= 1

in' :: UGen
in' = "in" @= 0

inBus :: UGen
inBus = "inBus" @= 0

lfoBus :: UGen
lfoBus = "lfobus" @= 0

mix :: UGen
mix = "mix" @= 0

mul :: UGen
mul = "mul" @= 0.1

mw :: UGen
mw = "mw" @= 0

out :: UGen
out = "out" @= 0

outBus :: UGen
outBus = "outBus" @= 0

pan :: UGen
pan = "pan" @= 0

pitch :: UGen
pitch = "pitch" @= 1

plfofreq :: UGen
plfofreq = "plfofreq" @= 1

postGain :: UGen
postGain = "postGain" @= 1

preGain :: UGen
preGain = "preGain" @= 1

preamp :: UGen
preamp = "preamp" @= 1

range :: UGen
range = "range" @= 1

rate :: UGen
rate = "rate" @= 1

rez :: UGen
rez = "rez" @= 1

rezz :: UGen
rezz = "rezz" @= 1

room :: UGen
room = "room" @= 0

rq :: UGen
rq = "rq" @= 0

start :: UGen
start = "start" @= 0

stretch :: UGen
stretch = "stretch" @= 1

sustain :: UGen
sustain = "sustain" @= 1

time :: UGen
time = "time" @= 1

trig :: UGen
trig = "trig" @= 1

val :: UGen
val = "val" @= 0