----------------------------------------------------------------------
-- | Parallel sequence studying, take4.
--
-- This time using @n_map@ to control triggers and parameters of ugen
-- which making the sound.
--
-- PROBLEMS:
--  * n_map is not working as expected.
--  * dbufrd is not returning expected values.
--

module ParallelSeq4 where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import Instances
import SimpleNotes

-- | Simple UGen for making sound.
para4UGen :: UGen
para4UGen = out 0 (pan2 osc pos 1) where
    osc = sinOsc ar freq 0 * amp
    pos = control kr "pan" 0.0
    freq = control kr "freq" 440
    amp = control kr "amp" (dbAmp (-20)) * env
    env = envGen kr envTrig 1 0 1 DoNothing envShape
    envTrig = control kr "trig" 0
    envShape = envPerc 0.01 0.8

-- | Make this controllable!
bpm :: Num a => a
bpm = 130

-- | UGen to send trigger.
trigUGen :: IO UGen
trigUGen = do
  let idx = in' 1 kr (control kr "idx" 0)
  durs <- dbufrd (control kr "durbuf" 0) idx NoLoop
  let bus = control kr "out" 100
      trigger = tDuty kr (60*durs/bpm) 0 RemoveSynth 1 0
      idx' = stepper trigger 0 0
             (bufFrames kr (control kr "durbuf" 0)) 1 0
  return $ mrg [out bus trigger, out (control kr "idx" 0) idx']

-- | UGen to send parameters.
paramUGen :: IO UGen
paramUGen = do
  let idx = in' 1 kr (control kr "idx" 0)
  params <- dbufrd (control kr "parambuf" 0) idx NoLoop
  let trigger = control kr "trig" 0
      param = demand trigger 0 params
      bus = control kr "out" 100
  return $ out bus param
  --     idx' = stepper trigger 0 0
  --            (bufFrames kr (control kr "parambuf" 0) - 1) 1 0
  -- return $ mce [out bus param, out (control kr "idx" 0) idx']

ampBus1,freqBus1,trigBus1,idxBus1 :: Num a => a
ampBus1 = 100
freqBus1 = 101
trigBus1 = 102
idxBus1 = 103

ampBus2,freqBus2,trigBus2,idxBus2 :: Num a => a
ampBus2 = 200
freqBus2 = 201
trigBus2 = 202
idxBus2 = 203

ampBuf1,durBuf1,freqBuf1 :: Num a => a
ampBuf1 = 10
durBuf1 = 11
freqBuf1 = 12

ampBuf2, durBuf2,freqBuf2 :: Num a => a
ampBuf2 = 20
durBuf2 = 21
freqBuf2 = 22

-- | Setup mappings, buffers, and sound making ugens.
setup :: IO ()
setup = do
  -- write synthdef files
  writeSynthdef "para4" para4UGen
  writeSynthdef "trig" =<< trigUGen
  writeSynthdef "param" =<< paramUGen
  withSC3 reloadSynthdef

  -- allocate buffers and fill with parameters
  withSC3 setupBuffers
  withSC3 soundUGenMappings

-- | Allocates buffers and fills with parameters.
--
-- XXX: Use bundle instead of calling async and send repeatedly.
--
setupBuffers :: Transport t => t -> IO ()
setupBuffers fd = do
  now <- utcr
  async fd (b_free ampBuf1)
  async fd (b_alloc ampBuf1 (length notes1) 1)
  async fd (b_free freqBuf1)
  async fd (b_alloc freqBuf1 (length notes1) 1)
  async fd (b_free durBuf1)
  async fd (b_alloc durBuf1 (length notes1) 1)

  send fd $ Bundle (UTCr (now + 1))
           [
            b_setn freqBuf1 [(0, map (midiCPS . notePitch) notes1)],
            b_setn durBuf1 [(0, map noteDur notes1)],
            b_setn ampBuf1
                       [(0, map (dbAmp . (flip (-) 100) . noteAmp) notes1)]
           ]

-- | Send sound generating ugens and node mapping messages.
soundUGenMappings :: Transport t => t -> IO ()
soundUGenMappings fd = do
  let nId1 = 1000
      nId2 = 1001
      paraAmp1 = 1002
      paraFreq1 = 1003
      paraAmp2 = 1004
      paraFreq2 = 1005
  now <- utcr
  send fd $ Bundle (UTCr now)
       [
        s_new "para4" nId1 AddToTail 1 [("pan",0.5)],
            n_map nId1 [("amp",ampBus1),("freq",freqBus1),("trig",trigBus1)],
        s_new "param" paraAmp1 AddToHead 1
                  [("out",ampBus1),("parambuf",ampBuf1),("idx",idxBus1)],
        n_map paraAmp1 [("trig",trigBus1)],
        s_new "param" paraFreq1 AddToHead 1
                  [("out",freqBus1),("parambuf",freqBuf1),("idx",idxBus1)],
        n_map paraFreq1 [("trig",trigBus1)],
        c_set [(idxBus1,0),(idxBus2,0)]
       ]

-- | Go with player ugen.
go :: IO ()
go = utcr >>= withSC3 . send' . bundle where
    bundle time = Bundle (UTCr time)
     [
      s_new "trig" 2001 AddToHead 1
                [("out",trigBus1),("durbuf",durBuf1),("idx",idxBus1)]
     ]
