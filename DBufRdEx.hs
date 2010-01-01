----------------------------------------------------------------------
-- | Scratch for dbufrd.
--

module DBufRdEx where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable

-- | Buffer for pitches.
pbufnum :: Num a => a
pbufnum = 1

-- | Buffer for durations.
dbufnum :: Num a => a
dbufnum = 2

dbufnum' :: Num a => a
dbufnum' = 3

-- | Allocate buffers and fill them.
prepare1 :: Transport t => t -> IO ()
prepare1 fd = do

  let ppattern = [60,63,65,67,63,65,63,58]
  async fd (b_alloc pbufnum 8 1)
  send fd (b_setn pbufnum [(0,ppattern)])

  let  dpattern = [1.5,1.0,0.5,0.5,0.5,0.5,1.5,0.5]
  async fd (b_alloc dbufnum 8 1)
  send fd (b_setn dbufnum [(0,dpattern)])

  async fd (b_alloc dbufnum' 8 1)
  send fd (b_setn dbufnum' [(0,dpattern)])

go1 :: Transport t => t -> IO ()
go1 fd = do
  let idx = 1
  durs <- dbufrd dbufnum idx Loop
  let trig = tDuty kr durs 0 DoNothing 1 0
      idx = stepper trig 0 0 7 1 0
  freqs <- dbufrd pbufnum (idx-1) Loop
  let freq = demand trig 0 freqs
      osc = sinOsc ar (midiCPS freq) 0 * 0.2 * env
      env = envGen kr trig 1 0 1 DoNothing shape
      shape = envPerc 0.6 0.605
  audition (out 0 (pan2 osc 0 1))
