{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

Module to contain example.

-}
module Scratch.Pattern.SPE where

import Control.Concurrent (threadDelay)
import System.Random (newStdGen, randomRs)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import Sound.SC3.Lepton.Pattern

main :: IO ()
main = withSC3 go

-- | Load synthdef and play the pattern.
go :: Transport t => t -> IO ()
go fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  mapLIO_ f pspe
  where
    f v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ 0.13 * 1e6)

-- | Synthdef for spe example.
speSynth :: IO UGen
speSynth = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ mkSig dl dr
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
    freq = control KR "freq" 440

-- Helpers
i = pint; d = pdouble; ds = map d
pir l h = pirange (i l) (i h); prnd = prand (i 1); prep = preplicate

-- Pattern used for pitches.
pspe =
  pcycle
    [prep (pir 0 1)
      (pconcat (ds [24,31,36,43,48,55]))
    ,prep (pir 2 5)
      (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
    ,prep (pir 3 9)
      (prnd (ds [74,75,77,79,81]))]
