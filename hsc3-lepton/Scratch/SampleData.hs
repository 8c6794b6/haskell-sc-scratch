{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Sample patterns.
-}
module Sound.SC3.Lepton.Scratch.SampleData where

import Control.Monad
import System.Random
import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Scratch.Bz
import Scratch.Client
import Scratch.ParseP (parsePattern)

l = withLept

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

pspeFreq =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate 1024 (1/41)
                   ,preplicate 512 (2/41)
                   ,preplicate 256 (4/41)
                   ,preplicate 128 (8/41)])
  ,("freq", midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  ,("n_map/fmul", pforever 100)]

loop02 = psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp $ prange (log 110) (log 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]

p01 = p01' (prange (-1) 1)

p01' pan = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [1, 0.51,0.49])
  ,("freq", pcycle [1980,1540,1320 ,1100,990,880, 660,440,330, 220,110,55])

  -- ,("freq",
  --   prepeat 2 * (midiCPS $ pcycle [60,64,67, 57,60,54, 62,65,69, 59,62,53]))

  ,("amp", pcycle [0.75,1,1.5,2,1,0.75])
  ,("pan", pforever pan)]

p02 pan = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [0.52,0.48,0.51,0.49])
  ,("freq", midiCPS $ pcycle [60,64,67,72,67,64,55,59,62,67,62,59])
  ,("amp", pcycle [0.5,0.75,1,1.25,1,0.75])
  ,("pan", pforever pan)]

-- ---------------------------------------------------------------------------
-- Communicating with pattern server

goP01s :: IO ()
goP01s = withLept $ \fd -> forM_ [1..10] $ \i -> do
  pan <- randomRIO (-1,1)
  send fd =<< bundle' 8 (0.001*2**fromIntegral i)
    [l_new ("p01_" ++ show i) (p01' (pval pan))]

-- ---------------------------------------------------------------------------
-- Testing behaviour of finite patterns

p03 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 1)
  ,("freq", plist [880,660,440,330,220])
  ,("amp", plist [0.5,0.4,0.5,0.4,0.3])]
