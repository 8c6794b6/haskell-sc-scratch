{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

Module to play with sustaining bass.

-}
module DroneBass where

import Sound.OSC
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.Pattern

setup'drnb = withSC3 $ do
  reset
  sendOSC . bundle immediately . map (d_recv . uncurry synthdef) $
    [("drnbBass",drnbBass),("drnbHi",drnbHi)]
  patchNode (nodify drnbN)

drnbN =
  grp 0
  [ grp 1
    [syn' drnBnid "drnbBass" ["out"*=0]]
  , grp 2
    (replicate 6 (syn "drnbBass" []))
  ]

drnBnid = 1000

drnbBass :: UGen
drnbBass =
  let sig = sum sigs * (("amp"@@0.3) `lag` 0.1)
      sigs = zipWith3 mkF (repeat frq) [1..8] ['a'..]
      frq = "freq"@@110 `lag2` 0.3
      mkF f p s = sinOsc AR (f*p) 0 * e s
      e x = cubed (clip (lfdNoise1 x KR pi) 0.5 1)
  in  out ("out"@@0) (mce2 sig sig)

drnbHi :: UGen
drnbHi =
  let sig = sinOsc AR ("freq"@@440) 0 * e * ("amp"@@0.3)
      e = envGen KR 1 1 0 1 RemoveSynth $
          Envelope [0,1,0.2,0] [5e-3,112e-3,320e-3] [EnvLin] Nothing Nothing
  in  out ("out"@@0) (pan2 sig ("pan"@@0) 1)

d = pdouble; ds = map d

t = 0.2

bp01 =
  pnset drnBnid
  [("dur", pforever (d (t*32)))
  ,("amp", pforever (d 0.3))
  ,("freq", pforever (pmidiCPS (prand (pint 1) (ds [24,31,36]))))
  ]


hp01 = let pitches = [60,64,67, 72,76,79] in
  psnew "drnbHi" Nothing AddToHead 1
  [("dur", pforever (d (t*8)))
  ,("amp", pforever (d 0.3))
  ,("freq", pforever (pmidiCPS (prand (pint 1) (ds pitches))))
  ]

mkhp01 ptchs =
  psnew "drnbHi" Nothing AddToHead 1
  [("dur", pforever (prand (pint 1) [d t, pconcat (ds [t*0.5,t*0.5])]))
  ,("amp", pforever (d 0.2))
  ,("freq", pforever (pmidiCPS (prand (pint 1) (ds ptchs))))
  ]

mkhp02 ptchs =
  let note x =
        psnew "drnbHi" Nothing AddToHead 1
        [("dur", prand (pint 1) [d t, pconcat [d (t*0.5),d (t*0.5)]])
        ,("amp", pforever (pdrange (d 0.05) (d 0.2)))
        ,("pan", pforever (pdrange (d (-0.25)) (d 0.25)))
        ,("freq", pforever (pmidiCPS (d x)))]
  in  pforever (ppar (map note ptchs))

mkbp02 ptchs =
  let note i x =
        pnset i
        [("dur", pforever (d (t*4)))
        ,("amp", pforever (pdrange (d 0.05) (d 0.2)))
        ,("freq", pforever (pmidiCPS (d x)))]
  in  pforever (ppar (zipWith note [2000..] ptchs))


addbp01 =
  leptseq =<< bundle' (t*8) 0 [l_new "bp" bp01]

addbp02 xs =
  let xs' = [a+b|a<-xs,b<-[60,72]] in
  leptseq =<< bundle' (t*8) 0 [l_new "bp" (mkbp02 xs')]

addhp01 xs =
  let xs' = [a+b|a<-xs,b<-[60,72]] in
  leptseq =<< bundle' (t*8) 0 [l_new "hp" (mkhp01 xs')]

addhp02 xs =
  let xs' = [a+b|a<-xs,b<-[60,72]] in
  leptseq =<< bundle' (t*8) 0 [l_new "hp" (mkhp02 xs')]

delhp = leptseq $ l_free "hp"
delbp = leptseq $ l_free "bp"

{-

withSC3 reset
withSC3 printRootNode
setup'drnb

delhp
delbp

leptseq l_dump
leptseq l_freeAll

addbp01

addbp02 [0,4,7] -- I

addhp02 [0,4,7] -- I
addhp02 [0,3,7] -- i

addhp02 [2,5,9] -- ii
addhp02 [2,6,9] -- II

addhp02 [4,7,11] -- iii
addhp02 [4,8,11] -- III

addhp02 [0,5,9] -- IV
addhp02 [0,5,8] -- iv

addhp02 [2,7,11] -- V
addhp02 [2,7,10] -- v

addhp02 [0,3,9] -- vi
addhp02 [1,3,9] -- VI

addhp02 [2,6,11] -- vii-5

addhp02 [2,5,8,11] -- dim
addhp02 [1,4,7,10] -- dim
addhp02 [0,3,6,9]  -- dim

addhp01 [0,4,8] -- I+
addhp01 [0,4,7] -- I
addhp01 [0,3,7] -- i


-- bp02

addbp02 [0,4,7] -- I
addbp02 [0,3,7] -- i

addbp02 [2,5,9] -- ii
addbp02 [2,6,9] -- II

addbp02 [4,7,11] -- iii
addbp02 [4,8,11] -- III

addbp02 [0,5,9] -- IV
addbp02 [0,5,8] -- iv

addbp02 [2,7,11] -- V
addbp02 [2,7,10] -- v

addbp02 [0,4,9] -- vi
addbp02 [1,4,9] -- VI

addbp02 [2,6,11] -- vii-5

addbp02 [2,5,8,11] -- dim
addbp02 [1,4,7,10] -- dim
addbp02 [0,3,6,9]  -- dim


-}
