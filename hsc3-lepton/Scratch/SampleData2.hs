{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Sample patterns, take 2.
-}
module Scratch.SampleData2 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

-- Motif pattern for frequency
pspeFreq =
  let d=pdouble; ds=map pdouble; i=pint in
  pcycle
    [pseq (pirange (i 0) (i 1))
       [pconcat (ds [24,31,36,43,48,55])]
    ,pseq (pirange (i 2) (i 5))
       [d 60, prand (i 1) (ds [63,65])
       ,d 67, prand (i 1) (ds [70,72,74])]
    ,prand (pirange (i 3) (i 9)) (ds [74,75,77,79,81])]

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever (pdouble 0.13))
  ,("amp", pforever (pdouble 0.22))
  ,("freq", pmidiCPS pspeFreq)]

-- SPE builder
mkspe x = let d=pdouble in
  psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever (d 0.13)),("amp", pforever (d 0.1)),("freq", x)]

mkspe' t a x = let d=pdouble in
  psnew "speSynth" Nothing AddToTail 1
  [("dur", pforever t),("amp", pforever a),("freq", x)]

-- Non-unison.
pspe2 = let x = pmidiCPS pspeFreq in
  ppar
  [ mkspe x
  , mkspe (x *@ pforever (pdouble 0.25))
  , mkspe (x *@ pforever (pdouble 2))]

-- Unison.
pspe3 =
  let d=pdouble; m=pmidiCPS pz
  in  plam tdouble (ppar [mkspe m,mkspe (m *@ d 0.25),mkspe (m *@ d 2)])
      `papp` pspeFreq

-- Inter octave.
pspe3a =
  let d=pdouble
  in  plam tdouble (mkspe (pmidiCPS $ pconcat [pz-@d 12,pz,pz+@d 12]))
      `papp` pspeFreq

-- Inter octave unison with repetation.
pspe3b =
  let d=pdouble; i=pint
  in  plam tdouble
      (ppar [mkspe' (d 0.13) (d 0.1) (pmidiCPS $ preplicate (i 4) (pz+@d 12))
            ,mkspe' (d 0.26) (d 0.1) (pmidiCPS $ preplicate (i 2) pz)
            ,mkspe' (d 0.52) (d 0.1) (pmidiCPS (pz-@d 24))])
      `papp` pspeFreq

-- Unison.
pspe4a =
  let d=pdouble; x=pmidiCPS pz
  in  plam tdouble
       (ppar
        [mkspe x
        ,mkspe (x *@ d 0.9925)
        ,mkspe (x *@ d 1.002)])
       `papp` pspeFreq

-- Non-unison.
pspe4b =
  let d = pdouble; x=pmidiCPS pspeFreq
  in  ppar
      [mkspe x
      ,mkspe (x *@ pforever (d 0.9925))
      ,mkspe (x *@ pforever (d 1.002))]

lll01 = let d = pdouble in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur", pforever (d 0.2))
  ,("amp", pforever (d 0.3))
  ,("pan", pforever $ pdrange (d (-1)) (d 1))
  ,("freq", pforever $ pdrange (d 100) (d 8000))
  ,("atk", pforever (d 1e-4))
  ,("dcy", pforever (d 1))
  ]

lll02 = let d = pdouble; i=pint in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur", pforever (d 0.4))
  ,("freq", pforever (d 880))
  ,("atk", pforever (pdrange (d 0.001) (d 1)))]

psw = pappend set03 (ppar [loop01, loop02, loop03])

psw' =
  withLept . flip send =<< bundle' 0 0
  [ l_new "set03" set03
  , l_new "loop01" loop01
  , l_new "loop02" loop02
  , l_new "loop03" loop03 ]

loop01 = let d=pdouble; i=pint in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate (i 1024) (d (1/41))
                   ,preplicate (i 512) (d (2/41))
                   ,preplicate (i 256) (d (4/41))
                   ,preplicate (i 128) (d (8/41))])
  ,("freq", pmidiCPS $ pforever $ prand (i 1) $
            map d [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ pdrange (d (-1)) (d 1))
  ,("atk",  pforever $ pdrange (d 1e-4) (d 1))
  ,("dcy",  pforever $ pdrange (d 1e-2) (d 1))
  ,("amp",  pforever $ pdrange (d 1e-3) (d 1))
  ,("n_map/fmul", pforever (d 100))]

loop02 = let d=pdouble in
  psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pforever $ pdrange (d 1e-1) (d 5e-1))
  ,("freq", pforever $ pexp $ pdrange (plog (d 110)) (plog (d 11000)))
  ,("atk",  pforever $ pdrange (d 1e-4) (d 2))
  ,("dcy",  pforever $ pdrange (d 1e-4) (d 2))
  ,("amp",  pforever $ pdrange (d 1e-2) (d 1))
  ,("pan",  pforever $ pdrange (d (-1)) (d 1))
  ,("q",    pforever $ pdrange (d 1e-3) (d 99e-2))]

loop03 = let d=pdouble in
  pnset 1003
  [("dur",    pforever $ pdrange (d 4) (d 32))
  ,("t_trig", pforever (d 1))]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pdouble 0.1)]

pfsm002 = pfsm [0]
  [(mkfsm02 [0,4,7],    [1,2,3,4])
  ,(mkfsm02 [2,5,9],    [3])
  ,(mkfsm02 [0,4,9],    [0,1,3,4])
  ,(mkfsm02 [2,5,7,-1], [0,4])
  ,(mkfsm02 [0,4,9],    [1,2,3])
  ]

mkfsm02 fs =
  let fs' = concatMap (\x -> map d [x+60,x+72{-,x+84-}]) fs
      d = pdouble; i = pint
  in psnew "rspdef1" Nothing AddToTail 1
     [("dur",  pforever (prand (i 1) (map d [0.125,0.125,0.125])))
     ,("freq", pmidiCPS $ pforever (prand (i 1) fs'))
     ,("atk",  pforever $ pdrange (d 1e-4) (d 3e-3))
     ,("dcy",  pforever $ pdrange (d 1e-1) (d 1))
     ,("pan",  pforever $ prand (i 1) (map d [-1,-0.5,0,0.5,1]))
     ,("amp",  preplicate (i 16) (pdrange (d 0.4) (d 0.6)))]
