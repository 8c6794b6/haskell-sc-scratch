{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Rewrite of ChuckOTF using L.

-}
module Scratch.Pattern.ChuckOTF2 where

import Sound.OpenSoundControl
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Client
import Sound.SC3.Lepton.Tree

import Scratch.Pattern.E
import Scratch.Pattern.Etree
import Scratch.Pattern.Deserialize
import Scratch.Pattern.L3
import Scratch.Pattern.PC02
import Scratch.ChuckOTF (setup'otf)

------------------------------------------------------------------------------
-- Patterns

kikP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (d t))
  ,("bnum", pforever (d 0))
  ,("pan", pforever (d (-0.1)))
  ,("amp", pforever (pdrange (d 0.7) (d 0.9)))]

snrP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (prand (i 1) [d (2*t), pconcat (ds [0.75*t,1.25*t])]))
  ,("bnum", pforever (d 1))
  ,("pan", pforever (d 0.3))]

hatP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (prand (i 1) [d (0.5*t), pconcat (ds [0.25*t,0.25*t])]))
  ,("bnum", pforever (d 2))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.3) (d 0.4)))]

hatoP = psnew "otfperc" Nothing AddToHead 1
  [("dur", pforever (d t))
  ,("bnum", pforever (d 3))
  ,("pan", pforever (d (-0.3)))
  ,("amp", pforever (pdrange (d 0.2) (d 0.4)))]

sin1P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (d (0.25*t)))
  ,("out", pforever (d 2))
  ,("freq",
    pforever $ pmidiCPS
    (d 21 +@ (prand (i 1) (ds [0,12,24,36]) +@ prand (i 1) (ds [0,2,4,7,9]))))
  ,("amp", pforever (d 0.65))
  ,("pan", pforever (d 0.1))]

sin2P = psnew "otfsine" Nothing AddToHead 1
  [("dur", pforever (prand (i 1) (ds [0.5*t,0.25*t])))
  ,("out", pforever (d 2))
  ,("freq",
    pforever $ pmidiCPS
    (d 69 +@ (prand (i 1) (ds [0,12,24,36]) +@ prand (i 1) (ds [0,2,4,7,9]))))
  ,("amp", pforever (d 0.2))
  ,("pan", pforever (d (-0.2)))]

t = 0.5
d = pdouble
ds = map pdouble
i = pint

sin1N = 0xbaca
sin2N = 0xcaba

------------------------------------------------------------------------------
-- Actions for patterns

addKik = addPat 0 "kik" kikP
addHat = addPat 0 "hat" hatP
addHato = addPat 0 "hat-open" hatoP
addSnr = addPat t "snr" snrP
addSin1 = addPat 0 "sin-lo" sin1P
addSin2 = addPat 0 "sin-hi" sin2P

dumpPat = withLept . flip send $ l_dump

addAll = sequence_ [addKik, addSnr, addHat, addHato, addSin1, addSin2]

addAllButSnare' = bundle' (t*2) 0
  [ l_new "kik" kikP, l_new "hat" hatP, l_new "hat-open" hatoP
  , l_new "sin-lo" sin1P, l_new "sin-hi" sin2P ]

resetAll = withLept (flip send l_freeAll)
addPat d n e = withLept . flip send =<< bundle' (t*2) d [l_new n e]
delPat n = withLept . flip send =<< bundle' (t*2) 0 [l_free n]

pausePat n = leptseq =<< bundle' (t*2) 0 [l_pause n]
runPat n = leptseq =<< bundle' (t*2) 0 [l_run n]
