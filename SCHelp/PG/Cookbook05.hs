------------------------------------------------------------------------------
-- |
-- Module      : SCHelp.PG.Cookbook05
-- Copyright   : 8c6794b6 <8c6794b6@gmail.com>
-- License     : BSD3
-- Maintainer  : 8c6794b6
-- Stability   : experimental
-- Portability : portable
--
-- Exercise for implementing pattern sequences shown in
-- /PG_Cookbook05_Using_Samples/.
--

module SCHelp.PG.Cookbook05
    ( -- * Playing a pattern in time with a sampled loop
      -- $runSampledLoop

      runSampledLoop,
      setSampledLoop,
      oneLoopBuf,
      runOneLoop,
      runBell0,
      runBell1,
      runBell2,

      -- ** UGens for playing sample and bell
      oneLoop,
      bell,

      -- ** Related example
      dsTest,
      dsHelp,
      runDsHelp,
      stHelp,

      -- * Using audio samples to play pitched material
      -- $runPitchedMaterial
      runPitchedMaterial,
      setPitchedMaterial,
      cleanPitchedMaterial,
      pitchedMaterialEvent,
      recordOneNote,
      samplerBuf,

      -- ** UGen for playing sample with specifyed pitch
      sampler,

      -- * Multi-sampled instruments
      -- $runMultiSampled
      runMultiSampled,
      setMultiSampled,
      tidyMultiSampled,

      -- ** Helper function and actions for multi-sampled ex
      baseBuf,
      baseFreqBuf,
      multiSampleMidiNotes,
      multiSampledEvents,
      recordSamples,
      fillInMidiNotes,
      testMultiSampled,
      playSampleSound,

      -- ** UGen for multi-sampled example
      multiSampler,
      sampleSource

    ) where

import Control.Applicative
    ((<$>),
     (<*>),
     pure )
import Control.Monad (replicateM)
import Data.List
import Data.Map (Map)
import Data.Monoid (mconcat)

import qualified Data.Map as M
import System.Random

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Math
import qualified Sound.SC3.Lang as L

import Missing
import Reusable
import SCSched
import SCTree
import SCQuery
import qualified Scratch.ControlArgs as A


main :: IO ()
main = runSampledLoop

-- $runSampledLoop
--
-- Exercise for looping the a11wlk01.wav sample between 0.404561 and
-- 3.185917 seconds, and overlay synthesized bells emphasizing the
-- meter.
--
-- This translation is not exact, since this haskell version is using
-- 3 event sequences for ringing bell tones, instead of 2 sequences
-- used in sclang version. The third sequence in haskell version is
-- for making the accent note.
--

-- | Plays a sequence of sampled sound and bell synth.
runSampledLoop :: IO ()
runSampledLoop = do
  runBell1' <- runBell1
  spawn 0 sampledLoopBPM $
        mconcat [runOneLoop, runBell0, runBell1', runBell2]

sampledLoopBPM :: Fractional a => a
sampledLoopBPM = 0.35953685899971 * 4 * 60

-- | Events for sample loop.
runOneLoop :: Event OSC
runOneLoop = listE $ zip durs msgs
    where
      durs = scanl (+) 0 $ repeat 4
      msgs = mkSNew' "oneLoop" 1 $
             M.fromList [("bufnum", repeat oneLoopBuf),
                         ("amp", repeat 0.4),
                         ("start", repeat 17841),
                         ("time", repeat $ recip 0.35953685899971)]

-- | Events for bells, main.
-- Stronger part of high frequency heard per every 4 beats.
runBell0 :: Event OSC
runBell0 = listE $ zip durs msgs
    where
      durs = scanl (+) 0.5 $ repeat 4
      msgs = mkSNew' "bell" 1 $
             M.fromList
              [("accent", repeat 2),
               ("amp", repeat 0.3),
               ("decayScale", repeat 6)]

-- | Events for bells, sub.
-- Weaker part of high frequency.
runBell1 :: IO (Event OSC)
runBell1 = listE <$> (zip <$> durs <*> pure msgs )
    where
      durs = scanl (+) 0.5 <$> dts
      dts = choices [0.25, 0.5, 0.75, 1] <$> newStdGen
      msgs = mkSNew' "bell" 1 $
             M.fromList
              [("accent", repeat 0),
               ("amp", repeat 0.1),
               ("decayScale", repeat 1)]

-- | Events for bells, sub.
-- Percussive low frequency tone.
runBell2 :: Event OSC
runBell2 = listE $ zip durs msgs
    where
      durs = scanl (+) 0.5 $ repeat 1
      msgs = mkSNew' "bell" 1 $
             M.fromList
              [("accent", repeat (-0.6)),
               ("amp", repeat 0.2),
               ("decayScale", repeat 0.1)]

-- | Setup for @runSampledLoop@. Update synthdefs and locate buffer.
setSampledLoop :: IO OSC
setSampledLoop =
    withSC3 $ \fd -> do
      (loadSynthdef "oneLoop" oneLoop) fd
      bell' <- bell
      (loadSynthdef "bell" bell') fd
      let a11wlk01 = "/home/atsuro/audio/wav/a11wlk01.wav"
      async fd (b_allocRead oneLoopBuf a11wlk01 0 0)

-- | UGen for playing sound file.
oneLoop :: UGen
oneLoop = out 0 $ mce [sig * e, sig * e]
    where
      sig = playBuf 1 A.bufnum 1 1 A.start NoLoop RemoveSynth
      e = envGen kr 1 1 0 1 RemoveSynth shp
      shp = envLinen 0.01 A.time 0.05 A.amp [EnvLin]

-- | Buffer id used for holding sound sample.
oneLoopBuf :: Num a => a
oneLoopBuf = 1

-- | UGen of fm bell.
bell :: IO UGen
bell = do
  exc <- (* decay2 (impulse kr 0 1) 0.01 0.05) <$> (* A.amp) <$>
         pinkNoise ar
  kFreq <- replicateM 4 $ expRand 400 1600
  kOffset <- replicateM 4 $ return 1
  kDecay <- replicateM 4 $ expRand 0.1 0.4
  let spec = klankSpec kFreq kOffset kDecay
      sig = klank exc (A.accent + 1) 0 A.decayScale spec
      d = detectSilence' sig 0.001 0.1 RemoveSynth
  return $ mrg [d, out 0 $ mce [sig, sig]]

-- | Translation of example shown in @DetectSilence@ sc help file.
-- Sends synthdef to scsynth.
dsHelp :: IO OSC
dsHelp = do
  osc <- sinOsc ar <$> rand 400 700 <*> pure 0
  z <- (* 0.8) <$> (* osc) <$> lfNoise2 kr 8
  let d = detectSilence' z 0.2 0.1 RemoveSynth
  withSC3 $ sendSynthdef "detectSilence-help" (mrg [d, out 0 z])

-- | Translation of example shown in @DetectSilence@ sc help file.
-- Spawns the task using synthdef defined in @dsHelp@.
runDsHelp :: IO ()
runDsHelp = spawn 0 60 =<< events
    where
      events :: IO (Event OSC)
      events = listE <$> (zip <$> durs <*> return msgs)
      durs :: IO [Double]
      durs = scanl (+) 0 <$> choices [0.5,1] <$> newStdGen
      msgs :: [OSC]
      msgs = repeat msg
      msg = s_new "detectSilence-help" (-1) AddToTail 1 []

-- | Copied from @detectSilence@ hsc3 help file .
dsTest :: IO ()
dsTest = do
 let { s = sinOsc AR 440 0 * mouseY KR 0 0.4 Linear 0.1
     ; d = detectSilence' s 0.1 0.2 RemoveSynth }
 audition (mrg [out 0 s, d])


-- $runPitchedMaterial
--
-- Plays sample in buffer with specifying rate for sample playback.
-- 
-- XXX: Calculation of frequency is wrong. Fix it.

-- | Runs pitched material.
runPitchedMaterial :: IO ()
runPitchedMaterial = spawn 0 60 =<< pitchedMaterialEvent

-- | Events for pitched material example.
pitchedMaterialEvent :: IO (Event OSC)
pitchedMaterialEvent = do
  degs <- choices [0..12] <$> newStdGen
  -- degs <- map (fromIntegral . fitInRange (-11) 11 . round) .
  --         scanl (+) 0 <$>
  --         choices [-2,-1,1,2] <$>
  --         newStdGen
  amps <- expRandomRs (0.1, 0.5) <$> newStdGen
  durs <- choices [0.25, 0.125] <$> newStdGen
  let durs' = scanl (+) 0 durs
      baseFreq = 440
      toFreq x = (freq $ defaultPitch {degree = x}) / baseFreq
      msgs =  mkSNew' "sampler" 1 $
              M.fromList
                   [("amp", amps),
                    ("freq", map toFreq degs),
                    ("bufnum", repeat samplerBuf)]
  return $ listE $ zip durs' msgs

-- | Set up task.
-- Loads synthdef and record sample to buffer.
setPitchedMaterial :: IO OSC
setPitchedMaterial = withSC3 $ \fd -> do
  loadSynthdef "sampler" sampler fd
  recordOneNote fd

-- | Clean up the resources used in pitched material example.
cleanPitchedMaterial :: IO ()
cleanPitchedMaterial = withSC3 $ \fd -> do
  send fd $ b_free samplerBuf

-- | Buffer used for pitched material example.
samplerBuf :: Num a => a
samplerBuf = 2

-- | Records a fm sound enveloped sound to buffer.
-- Don't understand why input ugen for recordBuf need to be increased
-- to 2 channels with @mce@, but scsynth will raise an error if not.
recordOneNote :: Transport t => t -> IO OSC
recordOneNote fd = do
  send fd $ b_alloc samplerBuf (44100 * 2) 1
  send fd $ sync 0
  let ug = recordBuf samplerBuf 0 1 0 1 NoLoop 1 (mce [car, car])
      car = sinOsc ar (freq + (mod * freq)) 0 * decay2 initPulse 0.01 2
      mod = sinOsc ar freq 0 * decay2 initPulse 0.01 3 * 5
      initPulse = impulse kr 0 0
      freq = 440
  play fd ug
  wait fd "/synced"

-- | UGen for pitched material example.
-- Plays specifyed buffer with specifyed rate.
sampler :: UGen
sampler = out A.out $ mce [sig, sig]
    where
      sig = playBuf 1 A.bufnum A.freq 1 0 NoLoop RemoveSynth * A.amp

-- $runMultiSampled
--
-- Changing pitch of sound with using sample, this time using multiple
-- samples. Synthdef differs from sclang example, haskell version has
-- distorted sound. Need to tweak inside the synthdef to figure out why.
--

-- | Main action of multi sampled example.
runMultiSampled :: IO ()
runMultiSampled = spawn 0 60 =<< multiSampledEvents

-- | Setup action for multi sample example.
setMultiSampled :: IO OSC
setMultiSampled = withSC3 $ \fd -> do
  loadSynthdef "multiSampler" multiSampler fd
  recordSamples fd
  fillInMidiNotes fd

-- | Tidy up buffers.
tidyMultiSampled :: IO OSC
tidyMultiSampled = withSC3 $ \fd -> do
  mapM_ (send fd) (map b_free [baseBuf .. (baseBuf + length
                                                   multiSampleMidiNotes)])
  send fd (sync 0)
  wait fd "/synced"

-- | Test
testMultiSampled :: Double -> Double -> IO ()
testMultiSampled bn f = withSC3 $ \fd -> do
  send fd $ s_new "multiSampler" (-1) AddToTail 1 
       [("out",0),
        ("amp",0.2),
        ("baseFreqBuf",baseFreqBuf),
        ("bufnum", bn),
        ("bufBase",baseBuf),
        ("freq",f)]

-- | Plays recorded sample sound.
playSampleSound :: Double -> IO ()
playSampleSound n = 
    audition $ out 0 $ playBuf 1 (constant n) 1 1 0 NoLoop RemoveSynth

-- | Midi notes for recorded samples.
multiSampleMidiNotes :: [Double]
multiSampleMidiNotes = [39,46..88]

-- | Base buffer of recorded sample sounds.
baseBuf :: Num a => a
baseBuf = 100

-- | Buffer to hold the base frequencies in midi note.
baseFreqBuf :: Num a => a
baseFreqBuf = 200

-- | OSC events of multi-sampled example.
multiSampledEvents :: IO (Event OSC)
multiSampledEvents = do
  durs <- scanl (+) 0 <$> choices [0.25, 0.125] <$> newStdGen
  degs <- map (fromIntegral . fitInRange (-11) 11 . round) <$> 
          scanl (+) 0 <$> choices [-2,-1,1,2] <$> newStdGen
  amps <- expRandomRs (0.1, 0.5) <$> newStdGen
  let bufBases = repeat baseBuf
      baseFreqBufs = repeat baseFreqBuf
      bufnums = map (\x -> L.indexInBetween x 
                           (map midiCPS multiSampleMidiNotes))
                     freqs
      toFreq d = freq $ defaultPitch {degree=d}
      freqs = map toFreq degs
  return $ listE $ zip durs $ mkSNew' "multiSampler" 1 $ M.fromList 
             [("amp", amps),
              ("baseFreqBuf", baseFreqBufs),
              ("bufBase", bufBases),
              ("bufnum", bufnums),
              ("freq", freqs),
              ("out", repeat 0)]

-- | Record sample notes with changing frequency.
recordSamples :: Transport t => t -> IO OSC
recordSamples fd = do
  let sendSampleSource b f = s_new "sampleSource" (-1) AddToTail 1 
                   [("bufnum",b),("freq",midiCPS f)]
  mapM_ (send fd) (zipWith3 b_alloc [baseBuf..]
                   (replicate (length multiSampleMidiNotes)
                    (44100 * 2))
                   (repeat 1))
  loadSynthdef "sampleSource" sampleSource fd
  mapM_ (send fd) (zipWith sendSampleSource [baseBuf..]
                           multiSampleMidiNotes)
  send fd (sync 0)
  wait fd "/synced"

-- | Fill in a buffer to hold the midi notes.
fillInMidiNotes :: Transport t => t -> IO OSC
fillInMidiNotes fd = do
    let l = length multiSampleMidiNotes
        fs = map midiCPS multiSampleMidiNotes
    send fd $ b_alloc baseFreqBuf l 1
    wait fd "/done"
    send fd $ b_setn baseFreqBuf [(0,fs)]
    send fd $ sync 1
    wait fd "/synced"

-- | UGen for multi sampled example.
multiSampler :: UGen
multiSampler = out A.out $ mce [sig, sig]
    where
      sig = xFade2 playbuf1 playbuf2 xfade A.amp
      buf1 = floorE A.bufnum
      buf2 = buf1 + 1
      xfade = mulAdd (A.bufnum - buf1) 2 (-1)
      basefreq1 = index A.baseFreqBuf buf1
      basefreq2 = index A.baseFreqBuf buf2
      playbuf1 = playBuf 1 (A.bufBase + buf1)
                 (A.freq / basefreq1) 1 0 NoLoop RemoveSynth
      playbuf2 = playBuf 1 (A.bufBase + buf2)
                 (A.freq / basefreq2) 1 0 NoLoop RemoveSynth

-- | Synth for sample sound
sampleSource :: UGen
sampleSource = recordBuf A.bufnum 0 1 0 1 NoLoop 1 (mce [car, car])
    where
      car = sinOsc ar (A.freq + (mod*A.freq)) 0 * decay2 initPulse 0.01 2
      mod = sinOsc ar A.freq 0 * decay2 initPulse 0.01 3 * 5
      initPulse = impulse kr 0 0
