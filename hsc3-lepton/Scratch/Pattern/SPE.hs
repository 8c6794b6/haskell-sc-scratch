{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6
Stability   : unstable
Portability : non-portable (NoMonomorphismRestriction)

Module to contain example.

-}
module Scratch.Pattern.SPE where

import Control.Concurrent (threadDelay)
import System.Random (newStdGen, randomRs)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

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
t = 0.13

-- Pattern used for pitches.
pspe =
  pcycle
    [prep (pir 0 1)
      (pconcat (ds [24,31,36,43,48,55]))
    ,prep (pir 2 5)
      (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
    ,prep (pir 3 9)
      (prnd (ds [74,75,77,79,81]))]

mkspe trans =
  psnew "speSynth" Nothing AddToTail 1
  [ ("dur", pforever (d t))
  , ("freq", pmidiCPS (pspe +@ pforever (d trans))) ]

mkspe2 trans rep =
  let freq = pmidiCPS (body +@ pforever (d trans))
      body = plam tdouble (preplicate (i rep) pz) `papp` pspe
  in  psnew "speSynth" Nothing AddToTail 1
      [ ("dur", pforever (pdouble t))
      , ("amp", pforever (pdouble 0.08))
      , ("freq", freq) ]

mkspe3 ts rs =
  let d=pdouble; m=pz
      fun x y =
        psnew "speSynth" Nothing AddToTail 1
        [ ("dur", pforever (pdouble t))
        , ("amp", pforever (pdouble 0.06))
        , ("freq", preplicate (pint y) (pmidiCPS pz *@ d x))
        ]
  in  plam tdouble (ppar (zipWith fun ts rs)) `papp` pspe

spe00 = mkspe 0
spe07 = mkspe 7
spe05 = mkspe 5
spe12 = mkspe 12

pbase =
  pconcat
    [prep (pir 0 1)
      (pconcat (ds [24,31,36,43,48,55]))
    ,prep (pir 2 5)
      (pconcat [d 60,prnd (ds [63,65]),d 67,prnd (ds [70,72,74])])
    ,prep (pir 3 9)
      (prnd (ds [74,75,77,79,81]))]

p01 =
  let ts = [0,7,5,2,7,0,4,7,5] in
  ppar
  [mkspe2 (-12) 64
  ,pforever $
   pconcat (map (preplicate (pirange (i 1) (i 4)) . mkbase) ts)]

mkbase trans =
  psnew "speSynth" Nothing AddToTail 1
  [ ("dur", pforever (d t))
  , ("freq", pmidiCPS (pbase +@ pforever (d trans))) ]

addspe name tran rep =
  leptseq =<< bundle' (t*8) 0 [l_new name (mkspe2 tran rep)]

addspe2 name trans reps =
  leptseq =<< bundle' (t*8) 0 [l_new name (mkspe3 trans reps)]

delspe name =
  leptseq =<< bundle' (t*8) 0 [l_free name]

{-

addspe2 "spe-u1" [0,-12,12] [1,32,16]
addspe2 "spe-u2" [0.25,1,1.5] [1,1,1]
delspe "spe-u2"
delspe "spe-u1"

delspe "spe2"

leptseq l_dump
leptseq l_freeAll

delspe "spe-lo"
delspe "spe-mid"
delspe "spe"
delspe "spe2"

delspe "spe-mid-2"

addspe "spe" 12 1
addspe "spe" 0 1
addspe "spe" 0 2
addspe "spe" 0 32
addspe "spe" 2 1
addspe "spe" 3 1
addspe "spe" 4 1
addspe "spe" 5 1
addspe "spe" 7 1
addspe "spe" 8 1
addspe "spe" 9 1

addspe "spe-lo" (-12) 128

addspe "spe2" 17 1
addspe "spe2" 19 1
addspe "spe2" 24 1
addspe "spe2" 29 1

addspe "spe-mid" 0 32
addspe "spe-mid" 0 8
addspe "spe-mid" 7 32
addspe "spe-mid" 4 32
addspe "spe-mid" 0 4
addspe "spe-mid" 0 2

leptseq l_freeAll

withSC3 $ flip send $ dumpOSC NoPrinter
withSC3 $ flip send $ dumpOSC HexPrinter
withSC3 printRootNode

-}
