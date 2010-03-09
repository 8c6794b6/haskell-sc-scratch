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
  sig <- undefined
  return $ out 0 $ mce [sig, sig]


-- $runPitchedMaterial

runPitchedMaterial :: IO ()
runPitchedMaterial = undefined

-- $runMultiSampled

runMultiSampled :: IO ()
runMultiSampled = undefined
