{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

The goal is, rewrite stream-pattern-events example with pattern in server
notification message send-receive style, without using threadDelay from
Control.Concurrent module.

-}
module RespSPE where

import Control.Applicative
import Control.Monad
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import qualified Data.Map as M
import qualified Data.Traversable as T

import Respond hiding (setup)

setup :: Transport t => t -> IO OSC
setup fd =
  async fd $ bundle immediately
    [d_recv $ synthdef "speSynth" speSynth
    ,d_recv $ synthdef "speSynth2" speSynth2]

-- | Synthdef used in /Streams Patterns Events/.
speSynth :: UGen
speSynth = out 0 $ mkSig dlyl dlyr where
  dlyl = randomRs (0,0.05) (mkStdGen 0xfd32810)
  dlyr = randomRs (0,0.05) (mkStdGen 0x1198732ab)
  mkSig ls rs = foldr f v (take 4 $ zipWith mce2 ls rs)
  v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
  f a b = allpassN b 0.05 a 4
  evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
  shp = envPerc 1e-4 1
  nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
  freq = "freq"@@440

-- | Variant of 'speSynth'.
speSynth2 :: UGen
speSynth2 = out 0 $ mkSig dlyl dlyr where
  dlyl = randomRs (0,0.05) (mkStdGen 0xaf3810)
  dlyr = randomRs (0,0.05) (mkStdGen 0x2398cd)
  mkSig ls rs = foldr f v (take 4 $ zipWith mce2 ls rs)
  f a b = allpassN b 0.05 a 4
  v =
    rlpf
      (lfSaw AR ("freq"@@440) 0)
      (midiCPS $ lfNoise1 'f' KR 1 * 36 + 110)
      0.1 *
    envGen KR 1 0.3 0 ("sustain"@@1) RemoveSynth (envPerc 1e-4 1)

pspe =
  pcycle
  [pchoose 1
   [pempty, plist [24,31,36,43,48,55]]
  ,pseq (prange 2 5)
   [60, pchoose 1 [63,65], 67, pchoose 1 [70,72,74]]
  ,pchoose (prange 3 9)
   [74,75,77,79,81]]

go :: Transport t => t -> IO ()
go =
  sNew AddToTail 1 "speSynth"
    [("dur",  prepeat 0.13)
    ,("freq", fmap midiCPS pspe)]

go2 :: Transport t => t -> IO ()
go2 =
  sNew AddToTail 1 "speSynth2"
    [("dur",     prepeat (1/7))
    ,("freq",    fmap midiCPS pspe)
    ,("sustain", prepeat (1/7))]

