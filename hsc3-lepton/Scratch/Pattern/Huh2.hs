{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Rewrite of Huh using pattern pattern classes in PC02.

-}
module Scratch.Pattern.Huh2 where

import Sound.SC3
import Sound.SC3.Lepton
import Scratch.Huh (n0)
import qualified Codec.Compression.Zlib as Z

main :: IO ()
main = withSC3 runHuh

runHuh fd = do
  reset fd
  patchNode n0 fd
  play fd $ toL allP

bpm = 295

allP = ppar
  [ huh1P, huh2P, huh3P
  , kikP, snrP, hatP
  , puP, drn1P, drn2P, bellP ]

------------------------------------------------------------------------------
-- Helpers

d = pdouble
ds = map pdouble
i = pint

mkSN def out key pat = mkSN' def out [(key,pat)]
mkSN2 def out key pat = mkSN' def out [("t_trig", pforever (d 1)),(key,pat)]
mkSN' def out kvs =
  psnew def Nothing AddToTail 10
  ([("dur", pforever (d (60/bpm))),("out", pforever (d out))] ++ kvs)

------------------------------------------------------------------------------
-- huhs

huh1P = pconcat (map (mkSN "cf2huh" 10 "t_trig") [huh1Pa, huh1Pb])
huh1Pa = pseq (i 4) (map d [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0])
huh1Pb =
  pcycle
   [pseq (i 12) (ds [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0])
   ,pseq (i 4)  (ds [0,1,0,1, 1,1,0,0, 0,0,0,1, 1,0,0,1])]

huh2P = pconcat (map (mkSN "cf2huh" 11 "t_trig") [huh2Pa, huh2Pb])
huh2Pa = pseq (i 16) (map d [0,0,0,0])
huh2Pb =
  pcycle
   [ pseq (i 12) (ds [1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0])
   , pseq (i 14) (ds [0,0,0,0])
   , pseq (i 4) (ds [0,1]) ]

huh3P = pconcat (map (mkSN "cf2huh" 12 "t_trig") [huh3Pa,huh3Pb])
huh3Pa = pseq (i 16) (ds [0,0,0,0])
huh3Pb =
  pcycle
  [ pseq (i 12) (ds [0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1])
  , pseq (i 14) (ds [0,0,0,0])
  , pseq (i 4) (ds [1,0]) ]

------------------------------------------------------------------------------
-- Percussive tones

kikP = pconcat (map (mkSN "cf2kik" 13 "t_trig") [kikPa,kikPb])
kikPa =
  pseq (i 4) (ds [1,0,0,0, 1,0,0,0, 0.8,0,0,0, 1,0,0,0])
kikPb =
  pcycle
  [ pseq (i 3)
    [ pseq (i 4)
      [ d 1, d 0, d 0,d 0, d 0.8
      , d 0, d 0, prand (i 1) (ds [0,0.7,0.8,1])
      , d 0.9, d 0,d 0,d 0
      , d 1, d 0,d 0, prand (i 1) (ds [0,0.7,0.8,1])]]
  , pseq (i 4)
    (ds [1,0,0,0.7, 1,0,0,1, 0,0.9,0,0.8, 0.9,0,0,1 ])]

snrP = pconcat (map (mkSN "cf2snr" 14 "t_trig") [snrPa,snrPb])
snrPa =
  pconcat
  [ pseq (i 56) (ds [0])
  , pconcat (ds [0.8,0.6,0,0.2, 0.2,0.8,0.4,1.0])]
snrPb =
  pcycle
  [ pseq (i 3)
    [ pseq (i 3)
      [ pseq (i 3) [d 0,d 0,pdrange (d 0.6) (d 1),d 0]
      , pconcat [d 0,d 0,pdrange (d 0.6) (d 0.8), pdrange (d 0.6) (d 0.8)]]
    , pconcat [pseq (i 2) [d 0,d 0,pdrange (d 0.6) (d 0.8),d 0]
              , prand (i 8) [d 0, d 0.5, d 0.75, d 1]]]
  , pseq (i 3) [ d 0, d 0, d 0, d 0
               , prand (i 1) [d 0.9, d 1.0], d 0, d 0, d 0
               , d 0, d 0, d 0, d 0
               , prand (i 4) (ds [1,0.8,0])]
  , pconcat [ d 0, d 0, d 0, d 0
            , prand (i 1) [d 0.9, d 1.0], d 0, d 0, d 0
            , prand (i 8) (ds [0,0,0,0.5,0.6,0.7,0.8,0.9,1])]]

hatP = pconcat (map (mkSN "cf2hat" 15 "t_trig") [hatPa,hatPb])
hatPa =
  pappend (pseq (i 62) (ds [0])) (pconcat (ds [0.6,0.8]))
hatPb =
  pcycle
  [ pseq (i 3)
    [ pseq (i 32)
      [ prand (i 1) (ds [0,0,0,0,0,0,0.2])
      , prand (i 1) (ds [0.5,0.8,1.0]) ]]
  , pseq (i 32) (ds [0])
  , pconcat
    [ pseq (i 30) (ds [0])
    , pconcat (ds [0.6,0.8]) ]]

------------------------------------------------------------------------------
-- Pitched tones

puP = mkSN2 "cf2pu" 16 "freq" (pmidiCPS puPa)
puPa =
  pcycle
   [ prand (i 7)
     [ pconcat (ds [36,55,62,36, 55,62,36,55])
     , pconcat (ds [36,60,72,36, 60,72,36,60])
     , pconcat (ds [36,53,58,36, 53,58,36,53]) ]
   , d 36, prand (i 2) (ds [60,67])
   , d 36, prand (i 2) (ds [67,72])
   , prand (i 2) (ds [48,53,55,60,65,67])]

drn1P =
  let f = pmidiCPS . pdouble; z = d 0 in
  pnset 1001
  [ ("dur", pforever (d (60/bpm)))
  , ("amp", pforever (d 0.3))
  , ("freq",
     pconcat
     [ pseq (i 32) (ds [0])
     , pcycle
       [ pseq (i 3)
         [ f 72, z, z, z,  z, z, z, z,  z,f 67, z, z,f 65, z, z, z
         , f 67, z, z, z,  z, z, z, z,  z, z, z, z, f 65, z, z, z
         , f 60, z, z, z,  z, z, z, z,  z,f 55, z, z, f 65, z, z, z
         , f 67, pseq (i 15) [z]]
       , pconcat
         [ f 72, pseq (i 31) [z]
         , f 60, pseq (i 31) [z]]]])]

drn2P =
  let f = pmidiCPS . pdouble; z = d 0 in
  pnset 1002
  [ ("dur", pforever (d (60/bpm)))
  , ("amp", pforever (d 0.3))
  , ("freq",
     pconcat
     [ pseq (i 32) (ds [0])
     , pcycle
       [ pseq (i 3)
         [ z, z, f 55,z, z, z, f 60, z,  z, z, z, z,  z,    z, z, z
         , z, z, z, z,   f 67, z, z, z,  z, z, z, z,  f 60, z, z, z
         , z, z, z, z,   z, z, f 67, z,  z, z, z, z,  z,    z, z, z
         , z, z, z, z,   f 60, z, z, z,  z, z, z, z,  z,    z, z, z]
       , pconcat
         [ z, z, f 55,z, pseq (i 28) [z]
         , z, z,    z,z, z, z, f 67, z, pseq (i 24) [z]]]]) ]

bellP = mkSN2 "cf2bell" 18 "freq" bellPa
bellPa =
  pconcat
  [ pseq (i 16) (ds [0,0,0,0])
  , pseq (i 6) (ds [0,0,0,0])
  , pcycle
    [ pmidiCPS $ prand (i 16) (map pdouble $ replicate 16 0 ++ [79,84,89,91,96])
    , pseq (i 12) (ds [0,0,0,0])]]
