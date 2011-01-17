------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Test for patterns, by playing it.
--
module P6Test where

import Control.Monad (zipWithM_)
import Control.Concurrent (threadDelay)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import P6

setup :: (Transport t) => t -> IO ()
setup fd = do
  ug' <- speSynth
  async fd $ d_recv $ synthdef "speSynth" ug'
  putStrLn "Synthdef sent" 
  
go :: (Transport t) => t -> IO ()  
go fd = do
  ps <- evalPIO pspe
  zipWithM_ f (repeat 0.25) ps
  where
    f t v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ t * (60/bpm) * 1e6)
    bpm = 120

speSynth :: IO UGen
speSynth = do 
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ g dl dr
  where
    g dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw ar freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen kr 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' kr 1 * 36 + 110)
    freq = control kr "freq" 440
