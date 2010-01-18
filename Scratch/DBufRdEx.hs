----------------------------------------------------------------------
-- | Scratch for dbufrd.
--
-- So, to use dbufrd ugen for sequeicing parameter for duration,
-- should I use external control bus to hold the current index of the
-- buffer, or use dseq instead of dbufrd.
--
-- From ghci, try:
-- > > withSC3 reset
-- > > withSC3 prepare1
-- > > withSC3 go1

module Scratch.DBufRdEx where

import Data.List (genericLength)

import Sound.SC3
import Sound.OpenSoundControl

import Reusable

-- | Buffer for pitches.
pitchBuf :: Num a => a
pitchBuf = 1

-- | Buffer for durations.
durBuf :: Num a => a
durBuf = 2

-- | Control bus to hold index for dbufrd.
idxbus :: Num a => a
idxbus = 100

-- | Control bus to hold bpm.
bpmbus :: Num a => a
bpmbus = 101

-- | Pattern for pitches.
ppattern :: [Double]
ppattern = [60,62,64,65,67,69,71,72]

-- | Pattern for durations.
dpattern :: [Double]
dpattern = [1.0,0.5,0.25,0.25,
            1.0,0.5,0.25,0.25]

-- | Allocate buffers and fill them.
prepare1 :: Transport t => t -> IO ()
prepare1 fd = do
  async fd (b_free pitchBuf)
  async fd (b_free durBuf)
  async fd $ b_alloc pitchBuf (genericLength ppattern)  1
  send fd $ b_setn pitchBuf [(0,ppattern)]
  async fd $ b_alloc durBuf (genericLength dpattern) 1
  send fd $ b_setn durBuf [(0,dpattern)]
  send fd $ c_set [(idxbus,0),(bpmbus,60)]

-- | Go for it.
go1 :: Transport t => t -> IO ()
go1 fd = do
  let bpm = in' 1 kr (control kr "bpm" bpmbus)
      idx = in' 1 kr (control kr "idx" idxbus)
  durs <- dbufrd durBuf idx Loop
  let trig = tDuty kr (60*durs/bpm) 0 DoNothing 1 0
      idx' = stepper trig 0 0 (genericLength ppattern - 1) 1 0
  freqs <- dbufrd pitchBuf idx Loop
  let freq = demand trig 0 freqs
      osc = sinOsc ar (midiCPS freq) 0 * 0.2 * env
      env = envGen kr trig 1 0 1 DoNothing shape
      shape = envPerc 0.06 0.605
  audition (mce [out 0 (pan2 osc 0 1),
                 out idxbus idx'])
