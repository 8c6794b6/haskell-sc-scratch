----------------------------------------------------------------------
-- | Scratch for playing with demand ugen and sequencing in parallel.
--
module Scratch where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import Instances
import SCTree

a = audition (out 0 (sinOsc ar 440 0 * 0.3))

b = do
  -- a pattern for pitch
  let pitchPat1 =
          [60,62,60,62, 64,62,64,62,
           60,62,60,62, 65,62,64,62,

           60,62,60,62, 64,62,64,62,
           67,65,64,62, 62,60,62,60]

  -- pattern for amplitude
  let ampPat1 =
          [1.0,0.4,0.8,0.5, 1.0,0.4,1.0,0.5,
           1.0,0.4,0.8,0.5, 1.0,0.4,1.0,0.5,
           1.0,0.4,0.8,0.5, 1.0,0.4,1.0,0.5,
           1.0,0.4,0.8,0.5, 1.0,0.4,1.0,0.5]

  -- another pattern for pitch
  let pitchPat2 =
          [60,60,60,60, 60,60,60,60,
           65,65,65,65, 65,65,67,67,
           60,60,60,60, 60,60,65,67,
           65,65,65,65, 65,65,67,67]

  -- pattern for amplitude
  let ampPat2 =
          [1.0,0.6,0.5,0.5, 1.0,0.4,0.3,0.5,
           0.8,0.3,0.3,0.3, 0.8,0.7,0.6,0.4,
           1.0,0.4,0.8,0.5, 0.7,0.4,0.6,0.5,
           0.9,0.3,1.0,0.5, 0.7,0.4,1.0,0.5]

  ds1 <- dseq 2 $ mce pitchPat1
  amp1 <- dseq 2 $ mce ampPat1

  ds2 <- dseq 2 $ mce pitchPat2
  amp2 <- dseq 2 $ mce ampPat2

  let bpm = 180
      clock = impulse KR (bpm/60) 0
      freqSeq1 = demand clock 0 ds1
      freqSeq2 = demand clock 0 ds2
      envTrig1 = tDuty KR (60/bpm) 1 RemoveSynth amp1 0
      envTrig2 = tDuty KR (60/bpm) 1 RemoveSynth amp2 0

      o1 = sinOsc AR (midiCPS freqSeq1) 0 * 0.3
      e1 = envGen KR envTrig1 envTrig1 0 1 DoNothing shape1
      shape1 = envPerc 0.01 0.8

      o2 = sinOsc AR (midiCPS (freqSeq2 - 12)) 0 * 0.4
      e2 = envGen KR envTrig2 envTrig2 0 1 DoNothing shape2
      shape2 = envPerc 0.03 0.5

      osc1 = o1 * e1
      osc2 = o2 * e2

  audition (out 0 (pan2 osc1 (-0.2) 1 +
                   pan2 osc2 0.3 1))


tree1 :: SCTree
tree1
    = Group 0
      [Group 1
       [Group 10
        [Synth 1000 "simpleTrigger"
         ["out" := v 100,"freq" := v 2]],
        Group 11
        [Synth 1100 "simplePercSine"
         ["trig" := bus 100,"out" := v 100],
         Synth 1101 "simplePercSine"
         ["trig" := bus 100,"out" := v 100,"freq" := v 660],
         Synth 1102 "simplePercSine"
         ["trig" := bus 100,"out" := v 101,"freq" := v 330],
         Synth 1103 "simplePercSine"
         ["trig" := bus 100,"out" := v 101,"freq" := v 220]],
        Group 12
        [Synth 1200 "simpleReverb" ["in" := v 100,"out" := v 0],
         Synth 1201 "simpleReverb" ["in" := v 101,"out" := v 1]]]]

v = PVal
bus = PBus
