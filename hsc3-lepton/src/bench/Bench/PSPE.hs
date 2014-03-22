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
module Bench.PSPE where

import Sound.SC3 (AddAction(..))
import Scratch.PC02

pspe2 = psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever (pdouble 0.13))
  ,("amp", pforever (pdouble 0.1))
  ,("freq", pmidiCPS pspeFreq2)]

pspeFreq2 =
  let d=pdouble; i=pint in
  pcycle
    [pseq (pirange (i 0) (i 1))
       [pconcat $ map d [24,31,36,43,48,55]]
    ,pseq (pirange (i 2) (i 5))
       [d 60, prand (i 1) [d 63,d 65]
       ,d 67, prand (i 1) [d 70,d 72,d 74]]
    ,prand (pirange (i 3) (i 9)) (map d [74,75,77,79,81])]
    

psw2 = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = let d = pdouble; i = pint in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate (i 1024) (d 1/@ d 41)
                   ,preplicate (i 512) (d 2 /@ d 41)
                   ,preplicate (i 256) (d 4 /@ d 41)
                   ,preplicate (i 128) (d 8 /@ d 41)])
  ,("freq", pmidiCPS $ pforever $ prand (i 1) $
            map d [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ pdrange (d (-1)) (d 1))
  ,("atk",  pforever $ pdrange (d 1e-4) (d 1))
  ,("dcy",  pforever $ pdrange (d 1e-2) (d 1))
  ,("amp",  pforever $ pdrange (d 1e-3) (d 1))
  ,("n_map/fmul", pforever (d 100))]

loop02 = let d=pdouble; i=pint in
  psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pforever $ pdrange (d 1e-1) (d 5e-1))
  ,("freq", pforever $ pexp $ pdrange (plog (d 110)) (plog (d 11000)))
  ,("atk",  pforever $ pdrange (d 1e-4) (d 2))
  ,("dcy",  pforever $ pdrange (d 1e-4) (d 2))
  ,("amp",  pforever $ pdrange (d 1e-2) (d 1))
  ,("pan",  pforever $ pdrange (d (-1)) (d 1))
  ,("q",    pforever $ pdrange (d 1e-3) (d 99e-2))]

loop03 = let d=pdouble; i=pint in
  pnset 1003
  [("dur",    pforever $ pdrange (d 4) (d 32))
  ,("t_trig", pforever (d 1))]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pdouble 0.1)]
       