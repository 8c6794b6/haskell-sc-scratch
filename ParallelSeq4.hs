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
-- TODO: 
--  * Write DSL for mapping nodes ... its tedious thing to do.
--  * Find efficient way for book keeping of nodeID, busID, buffeerID.

module ParallelSeq4 where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import SimpleNotes

import System.Random

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
    durs <- dbufrd (control kr "durbuf" 0) 0 NoLoop
    let bus = control kr "out" 100
        trigger = tDuty kr (60*durs/bpm) 0 RemoveSynth 1 0
    return $ out bus trigger 

-- | UGen to send parameters.
paramUGen :: IO UGen
paramUGen = do 
    params <- dbufrd (control kr "parambuf" 0) 0 NoLoop
    let trigger = control kr "trig" 0 
        param = demand trigger 0 params
        bus = control kr "out" 100
    return $ out bus param 

ampBus1,freqBus1,trigBus1 :: Num a => a
ampBus1 = 100
freqBus1 = 101
trigBus1 = 102

ampBus2,freqBus2,trigBus2 :: Num a => a
ampBus2 = 200
freqBus2 = 201
trigBus2 = 202

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

  -- allocate buffers and fill with parameters
  withSC3 (\fd -> do 
             reloadSynthdef fd
             setupBuffers fd
             soundUGenMappings fd)

-- | Allocates buffers and fills with parameters.
-- 
-- XXX: Use bundle instead of calling async and send repeatedly.
-- 
setupBuffers :: Transport t => t -> IO ()
setupBuffers fd = do

  async fd (b_free ampBuf1)
  async fd (b_free freqBuf1) 
  async fd (b_free durBuf1)
  async fd (b_free ampBuf2)
  async fd (b_free freqBuf2) 
  async fd (b_free durBuf2)

  async fd (b_alloc ampBuf1 (length notes1) 1) 
  async fd (b_alloc freqBuf1 (length notes1) 1)
  async fd (b_alloc durBuf1 (length notes1) 1)
  async fd (b_alloc ampBuf2 (length notes2) 1) 
  async fd (b_alloc freqBuf2 (length notes2) 1) 
  async fd (b_alloc durBuf2 (length notes2) 1)

  send fd $ b_setn ampBuf1 [
                   (0, map (dbAmp . (flip (-) 100) . noteAmp) notes1)]
  send fd $ b_setn freqBuf1 [(0, map (midiCPS . notePitch) notes1)]
  send fd $ b_setn durBuf1 [(0, map noteDur notes1)]

  send fd $ b_setn ampBuf2 [
                   (0, map (dbAmp . (flip (-) 100) . noteAmp) notes2)]
  send fd $ b_setn freqBuf2 [(0, map (midiCPS . notePitch) notes2)]
  send fd $ b_setn durBuf2 [(0, map noteDur notes2)]

-- | Send sound generating ugens and node mapping messages.
soundUGenMappings :: Transport t => t -> IO ()
soundUGenMappings fd = do
  let nId1 = 1000 
      nId2 = 1001
      paraAmp1 = 1002
      paraFreq1 = 1003
      paraAmp2 = 1004
      paraFreq2 = 1005
  send fd $ s_new "para4" nId1 AddToTail 1 [("pan",0.5)]
  send fd $ s_new "para4" nId2 AddToTail 1 [("pan",-0.5)]
  send fd $ n_map nId1 
           [("amp",ampBus1),("freq",freqBus1),("trig",trigBus1)]
  send fd $ n_map nId2 
           [("amp",ampBus2),("freq",freqBus2),("trig",trigBus2)]

  send fd $ s_new "param" paraAmp1 AddToHead 1 
           [("out",ampBus1),("parambuf",ampBuf1)]
  send fd $ n_map paraAmp1 [("trig",trigBus1)]

  send fd $ s_new "param" paraFreq1 AddToHead 1
           [("out",freqBus1),("parambuf",freqBuf1)]
  send fd $ n_map paraAmp1 [("trig",trigBus1)]

  send fd $ s_new "param" paraAmp2 AddToHead 1 
           [("out",ampBus2),("parambuf",ampBuf2)]
  send fd $ n_map paraAmp1 [("trig",trigBus2)]

  send fd $ s_new "param" paraFreq2 AddToHead 1
           [("out",freqBus2),("parambuf",freqBuf2)]
  send fd $ n_map paraAmp1 [("trig",trigBus2)]

-- | Go with player ugen.
go :: IO ()
go = utcr >>= withSC3 . send' . bundle where
    bundle time = Bundle (UTCr time) 
     [
      s_new "trig" 2001 AddToHead 1
                [("out",trigBus1),("durbuf",durBuf1)],
      s_new "trig" 2002 AddToHead 1
                [("out",trigBus2),("durbuf",durBuf2)]
     ]
                   
test1 = 
    let n = randomRs (200.0, 500.0) (mkStdGen 0)
    in do { withSC3 (\fd -> do { async fd (b_alloc 10 24 1)
                           ; send fd (b_setn 10 [(0, take 24 n)]) })
      ; s <- dseq 3 (mce [0, 3, 5, 0, 3, 7, 0, 5, 9])
      ; b <- dbrown 5 0 23 1
      ; p <- dseq dinf (mce [s, b])
      ; t <- dust KR 10
      ; r <- dbufrd 10 p Loop
      ; audition (out 0 (sinOsc AR (demand t 0 r) 0 * 0.1)) }
