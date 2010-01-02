{-# LANGUAGE ExistentialQuantification #-}
----------------------------------------------------------------------
-- | Parallel sequence studying, take4.
--
-- This time using @n_map@ to control triggers and parameters of ugen
-- which making the sound.
--
-- Try:
-- > > withSC3 reset
-- > > setup
-- > > go

module ParallelSeq4 where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import Instances
import SimpleNotes

-- | Simple UGen for making sound.
para4UGen :: UGen
para4UGen = out (control kr "out" 0) osc where
    osc = sinOsc ar freq 0 * amp
    freq = control kr "freq" 440
    amp = control kr "amp" (dbAmp (-20)) * env
    env = envGen kr envTrig 1 0 1 DoNothing envShape
    envTrig = control kr "trig" 0
    envShape = envPerc 0.01 (control kr "sustain" 0.8)

-- | Simple UGen for reverb effect, using freeVerb.
simpleReverv :: UGen
simpleReverv = out 0 result
    where result = freeVerb input mix room damp
          maxTime = 1.0
          input = in' 1 ar (control kr "in" 0)
          mix = control kr "mix" 0.5
          room = control kr "room" 0.5
          damp = control kr "damp" 0.5

-- | Simple panner.
simplePanner :: UGen
simplePanner = out 0 (pan2 input pan 1)
    where input = in' 1 ar (control kr "bus" 0)
          pan = control kr "pan" 0

-- | UGen to send trigger.
-- First trigger is executed before the output, control bus's initial value is
-- set to @-1@. And in this ugen, index would be one point prior to the other
-- parameters, hence added @1@ to the idx. ... True? Or, is there any other
-- way to avoid this offset fixing? 
--
-- XXX: 
--  * Read "Order of execution" SC help.
--  * Try implement this ugen with using LocalIn, LocalOut UGens.
-- 
trigUGen :: IO UGen
trigUGen = do
  let idx = in' 1 kr (control kr "idx" 0)
      bpm = control kr "bpm" 130
  durs <- dbufrd (control kr "durbuf" 0) (idx+1) Loop
  let bus = control kr "out" 100
      trigger = tDuty kr (60*durs/bpm) 0 DoNothing 1 0
      idx' = stepper trigger 1 0
             (bufFrames kr (control kr "durbuf" 0) - 1) 1 0
  return $ mrg [out (control kr "idx" 0) idx', out bus trigger]

-- | UGen to send parameters. 
-- Receives trigger from trigUGen, and read the specifyed buffer with dbufrd,
-- sends the value written in the buffer to out. Intended use of this ugen is
-- to map the output to other ugen, for sequencing parameters.
paramUGen :: IO UGen
paramUGen = do
  let idx = (control kr "idx" 0)
  params <- dbufrd (control kr "parambuf" 0) idx NoLoop
  let trigger = control kr "trig" 0
      param = demand trigger 0 params
      bus = control kr "out" 100
  return $ out bus param

--
-- Busses
--
ampBus1,freqBus1,trigBus1,durIdxBus1,effectBus1 :: Num a => a
ampBus1 = 100
freqBus1 = 101
trigBus1 = 102
durIdxBus1 = 103
effectBus1 = 104

ampBus2,freqBus2,trigBus2,durIdxBus2,effectBus2 :: Num a => a
ampBus2 = 200
freqBus2 = 201
trigBus2 = 202
durIdxBus2 = 203
effectBus2 = 204

bpmBus :: Num a => a
bpmBus = 1000

-- 
-- Buffers
--
ampBuf1,durBuf1,freqBuf1 :: Num a => a
ampBuf1 = 10
durBuf1 = 11
freqBuf1 = 12

ampBuf2, durBuf2,freqBuf2 :: Num a => a
ampBuf2 = 20
durBuf2 = 21
freqBuf2 = 22

-- 
-- Groups
-- 
trigGroup, paramGroup, synthGroup, effectGroup :: Num a => a
trigGroup = 2
paramGroup = 3
synthGroup = 4
effectGroup = 5

-- | Initial bpm
bpm :: Num a => a
bpm = 130

-- | Setup mappings, buffers, and sound making ugens.
setup :: IO ()
setup = do
  -- write synthdef files
  writeSynthdef "para4" para4UGen
  writeSynthdef "trig" =<< trigUGen
  writeSynthdef "param" =<< paramUGen
  writeSynthdef "simpleReverb" simpleReverv
  writeSynthdef "simplePanner" simplePanner
  withSC3 reloadSynthdef

  -- allocate buffers and fill with parameters
  withSC3 setupBuffers
  withSC3 soundUGenMappings

-- | Allocates buffers and fill with parameters.
setupBuffers :: Transport t => t -> IO ()
setupBuffers fd = do
  now <- utcr
  async fd (b_free ampBuf1)
  async fd (b_alloc ampBuf1 (length notes1) 1)
  async fd (b_free freqBuf1)
  async fd (b_alloc freqBuf1 (length notes1) 1)
  async fd (b_free durBuf1)
  async fd (b_alloc durBuf1 (length notes1) 1)

  async fd (b_free ampBuf2)
  async fd (b_alloc ampBuf2 (length notes2) 1)
  async fd (b_free freqBuf2)
  async fd (b_alloc freqBuf2 (length notes2) 1)
  async fd (b_free durBuf2)
  async fd (b_alloc durBuf2 (length notes2) 1)

  send fd $ Bundle (UTCr (now + 1))
           [b_setn freqBuf1 [(0, map noteToFreq notes1)],
            b_setn durBuf1 [(0, map noteToDur notes1)],
            b_setn ampBuf1 [(0, map noteToAmp notes1)],
            b_setn freqBuf2 [(0, map noteToFreq notes2)],
            b_setn durBuf2 [(0, map noteToDur notes2)],
            b_setn ampBuf2 [(0, map noteToAmp notes2)]]


-- | Send sound generating ugens and node mapping messages.
-- Dont know why but panning for notes2 sound is not working as expected.
soundUGenMappings :: Transport t => t -> IO ()
soundUGenMappings fd = do
  now <- utcr
  let nId1 = 1000
      nId2 = 1001
      paraAmp1 = 1002
      paraFreq1 = 1003
      paraAmp2 = 1004
      paraFreq2 = 1005
      revId1 = 1006
      revId2 = 1007
      panId1 = 1008
      panId2 = 1009

  -- Add groups in order.
  send fd $ Bundle (UTCr now)
       [g_new [(trigGroup,AddToTail,1)],
        g_new [(paramGroup,AddAfter,trigGroup)],
        g_new [(synthGroup,AddAfter,paramGroup)],
        g_new [(effectGroup,AddAfter,synthGroup)]]

  -- Add synths to appropriate groups.
  send fd $ Bundle (UTCr $ now+0.1)
           [
            s_new "param" paraAmp1 AddToTail paramGroup
                      [("out",ampBus1),("parambuf",ampBuf1)],
            s_new "param" paraFreq1 AddToTail paramGroup
                      [("out",freqBus1),("parambuf",freqBuf1)],
            s_new "param" paraAmp2 AddToTail paramGroup
                      [("out",ampBus2),("parambuf",ampBuf2)],
            s_new "param" paraFreq2 AddToTail paramGroup
                      [("out",freqBus2),("parambuf",freqBuf2)],

            s_new "para4" nId1 AddToTail synthGroup 
                      [("out",effectBus1)],
            s_new "para4" nId2 AddToTail synthGroup 
                      [("out",effectBus2),("sustain",2.0)],
            
            s_new "simpleReverb" revId1 AddToTail effectGroup
                      [("in",effectBus1),("room",0.9)],
            s_new "simpleReverb" revId2 AddToTail effectGroup
                      [("in",effectBus2),("mix",0.9)],
            s_new "simplePanner" panId1 AddToTail effectGroup
                      [("bus",effectBus1),("pan",-0.25)],
            s_new "simplePanner" panId2 AddToTail effectGroup
                      [("bus",effectBus2),("pan",0.25)]]

  -- Map and set control busses.
  send fd $ Bundle (UTCr $ now+0.2)
       [n_map nId1 [("amp",ampBus1),("freq",freqBus1),("trig",trigBus1)],
        n_map paraAmp1 [("trig",trigBus1),("idx",durIdxBus1)],
        n_map paraFreq1 [("trig",trigBus1),("idx",durIdxBus1)],
        n_map nId2 [("amp",ampBus2),("freq",freqBus2),("trig",trigBus2)],
        n_map paraAmp2 [("trig",trigBus2),("idx",durIdxBus2)],
        n_map paraFreq2 [("trig",trigBus2),("idx",durIdxBus2)],
        c_set [(durIdxBus1,-1),(durIdxBus2,-1),(bpmBus,bpm)]]

-- | Go with player ugen.
go :: IO ()
go = utcr >>= withSC3 . send' . bundle where
    trigId1 = 2001
    trigId2 = 2002
    bundle time = Bundle (UTCr $ time+0.1)
     [s_new "trig" trigId1 AddToTail trigGroup
                [("out",trigBus1),
                 ("durbuf",durBuf1),
                 ("idx",durIdxBus1)],
      s_new "trig" trigId2 AddToTail trigGroup
                [("out",trigBus2),
                 ("durbuf",durBuf2),
                 ("idx",durIdxBus2)],
      n_map trigId1 [("bpm",bpmBus)],
      n_map trigId2 [("bpm",bpmBus)]]
