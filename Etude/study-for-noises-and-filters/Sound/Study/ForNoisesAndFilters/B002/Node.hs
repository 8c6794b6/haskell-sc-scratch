{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable
-}
module Sound.Study.ForNoisesAndFilters.B002.Node where

import Sound.SC3
import Sound.SC3.Lepton hiding (s)
import Sound.Study.ForNoisesAndFilters.B002.Synthdef

go :: IO ()
go = withSC3 $ patchNode n0

w = withSC3
g = Group
s = Synth

-- XXX: Found a bug.
-- Patching OSC message made from below to empty node tree will add group 50
-- inside group 20.
n0 :: SCNode
n0 =
  g 0
  [g 1
   [g 10 -- master control
    [s 1001 "b002met"
     ["bpm":=196,"outt":=100,"outb":=101]
    ,s 1002 "noises"
     ["out":=3,"colour":=0]]
   ,g 20 -- controls and sources
    [g 200
     [s 2001 "quickNoiseC"
      ["outt":=201,"outf":=202,"t_trig":<-100]
     ,s 2002 "quickNoise"
      ["out":=20,"a_in":<=3,"t_trig":<-201,"freq":<-202
      ,"amax":=0.25,"amin":=1e-3,"dmax":=0.25,"dmin":=6.25e-2]]
    ,g 210
     [s 2100 "boscC"
      ["outg":=203,"outf":=204,"t_trig":<-100]
     ,s 2101 "bosc"
      ["out":=22,"a_in":<=3,"gt":<-203,"freq":<-204
      ,"ff":=17800,"fq":=0.9]]
    ,g 220
     [s 2200 "slowNoiseC"
      ["out":=205,"t_trig":<-100]
     ,s 2201 "slowNoise"
      ["out":=24,"a_in":<=3,"t_trig":<-205]]
    ,g 230
     [s 2300 "hatLike1C"
      ["out":=206,"t_trig":<-100]
     ,s 2301 "hatLike"
      ["out":=25,"a_in":<=3,"t_trig":<-206]]
    ,g 240
     [s 2400 "hatLike2C"
      ["out":=207,"t_trig":<-100]
     ,s 2401 "hatLike"
      ["out":=26,"a_in":<=3,"t_trig":<-207]]
    ,g 250
     [s 2500 "bhitC"
      ["out":=208,"t_trig":<-100]
     ,s 2501 "bhit"
      ["out":=27,"a_in":<=3,"t_trig":<-208]]]
   ,g 50 -- mix
    [s 5000 "b002amps"
     ["quickNoise":=3,"bosc":=0.75,"slowNoise":=4
     ,"hat1":=1,"hat2":=1,"bhit":=0.1]
    ,s 5001 "b002mix1" -- quickNoise
     ["out":=0,"a_in":<=20,"amp":<-800
     ,"pan":=(-0.1),"dtl":=1e-4,"dtr":=0]
    ,s 5002 "b002mix2" -- bosc
     ["out":=0,"a_inl":<=22, "a_inr":<=23,"amp":<-801]
    ,s 5003 "b002mix1" -- slowNoise
     ["out":=0,"a_in":<=24,"amp":<-802
     ,"pan":=(-0.1),"dtr":=1e-4,"dtl":=2.8e-4]
    ,s 5004 "b002mix1" -- hat1
     ["out":=0,"a_in":<=25,"amp":<-803
     ,"pan":=0.95,"dtr":=1.8e-3,"dtl":=0]
    ,s 5005 "b002mix1" -- hat2
     ["out":=0,"a_in":<=26,"amp":<-804
     ,"pan":=(-0.95),"dtr":=0,"dtl":=2e-3]
    ,s 5006 "b002mix1" -- bhit
     ["out":=0,"a_in":<=27,"amp":<-805
     ,"pan":=(-0.25),"dtr":=2e-4,"dtl":=0]]
   ,g 99 -- master
    [s 9901 "b002mst"
     ["amp":=0.2,"a_inl":<=0,"a_inr":<=1]]]]
