{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Bench mark for pattern serialization and deserialization.

-}
module SampleData where

import Sound.SC3
import Sound.SC3.Lepton

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

t = 0.5

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever t)
  ,("bnum", prepeat 3)
  ,("pan", prepeat (-0.3))
  ,("amp", pforever (prange 0.2 0.4))]

sin2P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (t * prand 1 [0.5,0.25]))
  ,("out", prepeat 2)
  ,("freq",
    pforever $ midiCPS (69 + (prand 1 [0,12,24,36] + prand 1 [0,2,4,7,9])))
  ,("amp", pforever 0.2)
  ,("pan", pforever (-0.2))]

