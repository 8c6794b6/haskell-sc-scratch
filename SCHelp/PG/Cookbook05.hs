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
      pitchedMaterialEvent,
      recordOneNote,
      samplerBuf,

      -- ** UGen for playing sample with specifyed pitch
      sampler,

      -- * Multi-sampled instruments
      -- $runMultiSampled
      runMultiSampled
    ) where

import Control.Applicative
    ((<$>),
     (<*>),
     pure )
import Control.Monad
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as M
import System.Random

import FRP.Reactive
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lang.Math

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

-- | Copied from @sendTrig@ hsc3 help file.
stHelp :: IO ()
stHelp = do
  s <- lfNoise0 kr 8
  let o = sinOsc ar (s * 200 + 500) 0 * 0.1
  audition $ mrg [sendTrig' s 0 s, out 0 o]

-- $runPitchedMaterial
--
-- Plays sample in buffer with specifying rate for sample playback.

-- | Runs pitched material.
runPitchedMaterial :: IO ()
runPitchedMaterial = spawn 0 60 =<< pitchedMaterialEvent

-- | Events for pitched material example.
pitchedMaterialEvent :: IO (Event OSC)
pitchedMaterialEvent = do
  degs <- choices [0..12] <$> newStdGen
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

setPitchedMaterial = do
  withSC3 $ loadSynthdef "sampler" sampler

samplerBuf :: Num a => a
samplerBuf = 2

recordOneNote = undefined

sampler :: UGen
sampler = out A.out $ mce [sig, sig]
    where
      sig = playBuf 1 A.bufnum A.freq 1 0 NoLoop RemoveSynth * A.amp

-- $runMultiSampled
--
-- TBW

runMultiSampled :: IO ()
runMultiSampled = undefined
