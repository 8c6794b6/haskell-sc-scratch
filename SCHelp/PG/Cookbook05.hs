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

      -- ** UGens for playing sample and bell
      oneLoop,
      bell,

      -- * Using audio samples to play pitched material
      -- $runPitchedMaterial
      runPitchedMaterial,

      -- * Multi-sampled instruments
      runMultiSampled
    ) where

import Control.Applicative ((<$>))
import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3

import Reusable
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

-- | Plays a sequence of sampled sound and bell synth.
runSampledLoop :: IO ()
runSampledLoop = undefined

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

-- | Buffer id used for holding a11wlk01.wav.
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
      d = detectSilence sig 0.1 0.2 RemoveSynth
  return $ out 0 $ mce [sig, sig]

-- $runPitchedMaterial

runPitchedMaterial :: IO ()
runPitchedMaterial = undefined

-- $runMultiSampled

runMultiSampled :: IO ()
runMultiSampled = undefined
