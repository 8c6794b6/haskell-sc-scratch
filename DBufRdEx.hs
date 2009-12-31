----------------------------------------------------------------------
-- | Scratch for dbufrd.
--

module DBufRdEx where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable

bufnum :: Num a => a
bufnum = 1

prepare1 :: Transport t => t -> IO ()
prepare1 fd = do
  let pattern = [60,63,65,67,63,65,63,58]
  async fd (b_alloc bufnum 8 1)
  send fd (b_setn bufnum [(0,pattern)])

go1 :: Transport t => t -> IO ()
go1 fd = do 
  let trig = impulse kr 1 0 
      idx = pulseCount trig 0
  freqs <- dbufrd bufnum (idx-1) Loop
  let freq = demand trig 0 freqs
      osc = sinOsc ar (midiCPS freq) 0 * 0.2 * env
      env = envGen kr trig 1 0 1 DoNothing shape
      shape = envPerc 0.05 0.8
  audition (out 0 (pan2 osc 0 1))

  



  