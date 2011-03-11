{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with noises and filters, scratch 2.
--
module Sound.Study.ForNoisesAndFilters.Scratch2 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

setupRG002 fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("nz001",nz001),("nz002",nz002),("nz003",nz003),("nz004",nz004)
    ,("rng001",rng001),("cmb001",cmb001),("pan001",pan001),("rvb001",rvb001)
    ,("lmt001",lmt001),("hit002",hit002),("tr001",tr001)]

goRG002 fd = addNode 0 rgGraph fd

rgGraph =
  grp 1
    [grp 10
       [syn 1001 "tr001" ["out":=100,"freq":=8]]
    ,grp 20
       [syn 2001 "nz003" []]
    ,grp 21
       [syn 2101 "hit002" ["a_in":<=0,"t_trig":<-100]
       ,syn 2102 "rng001" ["a_in":<=0]
       ,syn 2103 "cmb001" ["a_in":<=0,"t_trig":<-100]
       -- ,syn 2104 "rvb001" ["a_in":<=0]
       ,syn 2105 "lmt001" ["a_in1":<=0,"a_in2":<=1]
       ,syn 2106 "pan001" ["a_in":<=0,"amp":=0.8]]
    ,grp 22
       []
    ,grp 30 
       []]

tr001 = out ("out"@@0) t where
  t = impulse kr ("freq"@@8) 0

nz001 = out 0 $ whiteNoise 'w' ar

nz002 = out 0 $ pinkNoise 'p' ar

nz003 = out 0 sig where
  sig = lorenzL ar sampleRate n0 n1 n2 0.05 0.1 0 0 * ("amp"@@1)
  n0 = n * 8 + 10
  n1 = n * 30 + 38
  n2 = n * 1.5 + 2
  n = lfdNoise3 'n' kr 1

nz004 = out 0 sig where
  sig = blip ar f 100
  f = lfdNoise3 'f' kr 0.25 * 1200 + 1210

hit002 = replaceOut 0 o where
  o = i * hit
  hit = envGen kr tr 1 0 dur DoNothing shape
  shape = env [1e-9,1e-9,1,1e-9] [0,atk,1-atk] [EnvExp] (-1) 0
  atk = linExp (lfdNoise3 'k' kr (1/16) * 0.5 + 0.51) 0.1 1.01 1e-4 9999e-4
  dur = linExp (lfdNoise3 'd' kr (1/16) * 0.5 + 0.51) 0.1 1.01 5e-3 2
  amp = tExpRand 'a' 0.1 1 tr
  tr = coinGate 'd' prob itr + dust 't' kr dtf
  itr = "t_trig"@@100
  prob = linExp (lfdNoise3 'p' kr (1/32) * 0.5 + 0.51) 0.1 1.01 (1/3) 1
  dtf = linLin (lfdNoise3 'r' kr (1/32) * 0.5 + 0.51) 0.1 1.01 1e-2 3
  i = "a_in"@@0

rng001 = mrg [replaceOut 0 o, out 1 o] where
  o = sum $ zipWith3 h fs ts as
  h freq time amp = ringz sig (lag2 freq 8) time * amp
  fs = [lfdNoise3 'a' kr (1/128) * 100 + 150
       ,lfdNoise3 'b' kr (1/128) * 300 + 500
       ,lfdNoise3 'c' kr (1/128) * 1200 + 2000
       ,lfdNoise3 'd' kr (1/128) * 4800 + 8000]
  ts = [lfdNoise3 'b' kr 1 * 0.5 + 0.6
       ,lfdNoise3 'd' kr 1 * 0.8 + 0.9
       ,lfdNoise3 'a' kr 1 * 0.5 + 0.6
       ,lfdNoise3 'c' kr 1 * 0.8 + 0.9]
  as = [lfdNoise3 'c' kr 1 * 0.25 + 0.24
       ,lfdNoise3 'a' kr 1 * 0.25 + 0.24
       ,lfdNoise3 'b' kr 1 * 0.25 + 0.24
       ,lfdNoise3 'd' kr 1 * 0.25 + 0.24]
  tt i lo hi = tExpRand i lo hi (dust 't' kr 0.1)
  sig = "a_in"@@0

cmb001 = replaceOut 0 o where
  o = foldr f sig [1..21::Int]
  f a b = combC b 0.5 (dlt a) (dct a)
  dlt i = lag3 (tExpRand i (recip 50) 1 tr) 28e-3
  dct i = lag3 (tExpRand i 120e-3 800e-3 tr) 28e-3
  tr = coinGate 't' prob tin
  prob = linLin (lfdNoise3 'q' kr (1/64) * 0.5 + 0.51) 0.1 1.01 (1/32) (1/2)
  tin = "t_trig"@@100
  sig = "a_in"@@0
  
rvb001 = mrg [lo, replaceOut 0 (mix output)] where
  output = (input * dry) + (mce [delrd !!* 0, delrd !!* 1] * (1-dry))
  dry = linLin (sinOsc kr dryf 0) (-1) 1 0 1
  dryf = linLin (lfdNoise3 'w' kr (1/128)) (-1) 1 (1/32) 4096
  delrd = localIn 4 ar
  sig0 = mce [output !!* 0, output !!* 1, delrd !!* 2, delrd !!* 3]
  sig1 = mix $ mceEdit fn sig0
  sig2 = sig1 * mce [0.4, 0.37, 0.333, 0.3]
  fn = zipWith (*) (map mce mtx)
  mtx = [[1,  1,  1,  1]
        ,[1, -1,  1, -1]
        ,[1,  1, -1, -1]
        ,[1, -1, -1,  1]]
  deltimes = mce [101, 143, 165, 177] * 0.001 - (1/controlRate)
  lo = localOut $ delayC sig2 deltimes deltimes
  input = "a_in"@@0

lmt001 = replaceOut 0 o where
  o = limiter (rhpf (mix $ i1+i2) 50 0.5 * 0.3) 1 0.2
  i1 = "a_in1"@@0
  i2 = "a_in2"@@1

pan001 = replaceOut 0 $ (c + mce [l,r]) * ("amp"@@1) where
  c = pan2 i (lfdNoise3 'p' kr (1/16)) 1
  l = delayC i 1 (lfdNoise3 'l' kr (1/16) * 20e-3 + 2001e-5)
  r = delayC i 1 (lfdNoise3 'r' kr (1/16) * 20e-3 + 2001e-5)
  i = "a_in"@@0

-- Probably better to add these 2 functions to lepton.
grp = Group
syn = Synth

w = withSC3

-- | Synonym operator of @mceChannel@
(!!*) :: UGen -> Int -> UGen
ug !!* n = mceChannel n ug

infixl 7 !!*

