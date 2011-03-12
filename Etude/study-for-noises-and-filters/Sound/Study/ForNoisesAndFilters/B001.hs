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
-- Playing with noises and filters, take 1
--
-- * Current best recording: b001-06.wav. Based on this, recorded b001-13.wav.
--
module Sound.Study.ForNoisesAndFilters.B001 where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

setup fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("wn001",wn001),("pn001",pn001),("lz001",lz001),("hn001",hn001)
    ,("bn001",bn001)
    ,("cg001",cg001),("cp001",cp001)
    ,("rng001",rng001),("cmb001",cmb001),("cmb002",cmb002)
    ,("pan001",pan001),("pan002",pan002),("pan003",pan003)
    ,("rvb001",rvb001)
    ,("lmt001",lmt001),("hit001",hit001),("tr001",tr001),("lzf001",lzf001)
    ,("hit002",hit002),("rzn001",rzn001)
    ,("mix001",mix001)]

go fd = addNode 0 rgGraph fd

rgGraph =
  grp 1
    [grp 10
       [syn 1001 "tr001"
          ["out":=100,"freq":=6]
       ,syn 1002 "lzf001"
          ["out":=101]]
    ,grp 20
       [syn 2001 "lz001"
          ["out":=0,"freq":<-101]]
    ,grp 21
       [syn 2101 "hit001"
          ["a_in":<=0,"out":=2,"t_trig":<-100]
       ,syn 2102 "rng001"
          ["a_in":<=2,"out_1":=2,"out_2":=3]
       ,syn 2103 "cmb001"
          ["a_in":<=2,"out":=2,"t_trig":<-100,"mix":=1]
       ,syn 2105 "lmt001"
          ["a_in1":<=2,"a_in2":<=3,"out":=2]
       ,syn 2106 "pan001"
          ["a_in":<=2,"out":=2,"amp":=0.9]]
    ,grp 22
       [syn 2201 "hit002"
          ["a_in":<=0,"out":=4,"t_trig":<-100,"lamp":=14,"mamp":=4,"hamp":=1]
       ,syn 2202 "lmt001"
          ["a_in1":<=4,"a_in2":<=4,"out":=4]
       ,syn 2203 "pan002"
          ["a_in":<=4,"out":=4,"amp":=1]]
    ,grp 23
       [syn 2301 "cmb002"
          ["a_in":<=0,"out":=6,"t_trig":<-100]
       ,syn 2302 "pan003"
          ["a_in":<=6,"out":=6,"amp":=0.125]]
    ,grp 30
       [syn 3000 "mix001"
         ["a_l1":<=2,"a_r1":<=3,"a_l2":<=4,"a_r2":<=5
         ,"a_l3":<=6,"a_r3":<=7,"mamp":=0.8]]]

tr001 = mrg [d, out ("out"@@0) t] where
  t = impulse kr ("freq"@@8) 0
  d = line kr 0 1 215 RemoveSynth

lzf001 = out ("out"@@101) (o+20) where
  o = envGen kr 1 (sampleRate-20) 0 1 RemoveSynth shp
  shp = env [0,0,1,0.5,1,0.125,1,1,0] [0,30,15,30,15,30,60,30]
        [EnvCub] (-1) 0

wn001 = out ("out"@@0) $ whiteNoise 'w' ar

pn001 = out ("out"@@0) $ pinkNoise 'p' ar

gn001 = out ("out"@@0) $ grayNoise 'g' ar

bn001 = out ("out"@@0) $ brownNoise 'b' ar

lz001 = out ("out"@@0) sig where
  sig = lorenzL ar ("freq"@@48000) n0 n1 n2 0.05 0.1 0 0 * ("amp"@@1)
  n0 = n * 2 + 10
  n1 = n * 20 + 38
  n2 = n * 1.5 + 2
  n = lfdNoise3 '0' kr 1

hn001 = out ("out"@@0) sig where
  sig = henonC ar hf n0 n1 0 0
  hf = ("freq"@@24000)
  n0 = n * 0.2 + 1.2
  n1 = n * 0.1 + 0.25
  n = lfdNoise3 'h' kr 1

cg001 = out ("out"@@0) sig where
  sig = linCongC ar f n0 n1 n2 0
  f = line kr 1 (sampleRate/2) 128 DoNothing
  n0 = n * 1e-4 + 1e-4
  n1 = n * 0.5 + 1.4
  n2 = n * 0.1 + 0.1
  n = lfdNoise3 'n' kr 1

cp001 = out ("out"@@0) o where
  o = cuspL ar f n0 n1 0
  f = line kr 1 (sampleRate/2) 128 DoNothing
  n0 = n * 0.5 + 0.6
  n1 = n * 0.9 + 1.1
  n = lfdNoise3 'n' kr 1

lc001 = out ("out"@@0) o where
  o = latoocarfianC ar f a b c d 0.5 0.5
  f = ("freq"@@24000)
  a = n 'a' * 1.5 + 1.5
  b = n 'b' * 1.5 + 1.5
  c = n 'c' * 0.5 + 1.5
  d = n 'd' * 0.5 + 1.5
  n i = lfdNoise3 'n' kr 1

ck001 = out ("out"@@0) $ crackle ar 1.95

hit001 = replaceOut ("out"@@0) o where
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

hit002 = replaceOut ("out"@@0) o where
  o = low + mid + high
  low = lowi * lowe * 20
  lowe = envGen kr trl 1 0 2600e-3 DoNothing $
         env [0,0,1,0] [0,5e-3,995e-3] [EnvNum (-13)] (-1) 0
  mid = midi * mide * 10 * tExpRand 'm' 0.5 1 trm
  mide = envGen kr trm 1 0 300e-3 DoNothing $
         env [0,0,1,0] [0,edgem,1-edgem] [EnvNum (-13)] (-1) 0
  edgem = index durb (tiRand 'm' 0 4 trm)
  high = highi * highe * 8 * tExpRand 'h' 0.8 1 trh
  highe = envGen kr trh 1 0 300e-3 DoNothing $
          env [0,0,1,0] [0,edgeh,1-edgeh] [EnvNum (-13)] (-1) 0
  edgeh = index durb (tiRand 'h' 0 4 trm)
  trl = mix $ coinGate 'l' 0.9375 $ pulseDivider tin 64 (mce [0,32,34])
  trm = mix $ pulseDivider tin 16 (mce [0,3,6,8,12])
  trh = coinGate 'h' 0.75 $ pulseDivider tin 1 0
  lowi = mix $ resonz i (mce [81,109,123,157]) 0.1 * ("lamp"@@1)
  midi = mix $ resonz i (mce [807,987,1103,1203]) 0.3 * ("mamp"@@1)
  highi = mix $ resonz i (mce [3977,6301,8409,12093]) 0.4 * ("hamp"@@1)
  durb = asLocalBuf 'd' durs
  durs = [1e-3,2e-3,3e-3,4e-3,995e-3]
  tin = "t_trig"@@100
  i = "a_in"@@0

rng001 = mrg [replaceOut ("out_1"@@0) o, out ("out_2"@@1) o] where
  o = fadeOutEnv * (sum $ zipWith3 h fs ts as)
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

rzn001 = replaceOut ("out"@@0) o where
  o = resonz i freq rq * ("amp"@@1)
  freq = 200
  rq = linLin (lfdNoise3 'q' kr (1/32)) (-1) 1 1e-9 1
  i = "a_in"@@0

cmb001 = replaceOut ("out"@@0) o where
  o = (c*m) + (sig * (1-m))
  c = foldr f sig [1..16::Int]
  f a b = combC b 0.5 (dlt a) (dct a)
  dlt i = lag3 (tExpRand i (recip 50) 1 tr) 28e-3
  dct i = lag3 (tExpRand i 120e-3 800e-3 tr) 28e-3
  tr = coinGate 't' prob tin
  prob = linLin (lfdNoise3 'q' kr (1/64) * 0.5 + 0.51) 0.1 1.01 (1/32) (1/2)
  m = lag ("mix"@@0.5) 0.2
  tin = "t_trig"@@100
  sig = "a_in"@@0

cmb002 = replaceOut ("out"@@0) o where
  o = sum [mkO 'z' 230e-3, mkO 'y' 330e-3, mkO 'x' 120e-3]
  mkO j dt = combL i 0.5 (1/f) 0.25 where
    f = lag2 (index freqB (tiRand j 0 7 tr)) dt
  freqB = asLocalBuf 'a' ps
  ps = map (midiCPS . (+48)) [0,2,4,5,7,9,11,12]
  tr = pulseDivider tri 64 0
  tri = "t_trig"@@100
  i = "a_in"@@0

rvb001 = mrg [lo, replaceOut ("out"@@0) (mix output)] where
  output = (input * dry) + (mce [delrd !!* 0, delrd !!* 1] * (1-dry))
  dry = linLin (sinOsc kr dryf 0) (-1) 1 0 1
  dryf = linLin (lfdNoise3 'w' kr (1/128)) (-1) 1 (1/16) 1024
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

lmt001 = replaceOut ("out"@@0) o where
  o = limiter (rhpf (mix $ i1+i2) 50 0.5 * 0.3) 1 0.2
  i1 = "a_in1"@@0
  i2 = "a_in2"@@1

pan001 = mkPan001 'r' 'l'

pan002 = mkPan001 's' 'm'

pan003 = mkPan001 'n' 't'

mkPan001 lid rid = replaceOut ("out"@@0) o where
  -- o = (c + mce [l,r]) * a
  o = mce [l,r] * a
  (l,r) = (f lid,f rid)
  -- c = pan2 i (lfdNoise3 (fromEnum lid * fromEnum rid) kr (1/exp pi)) 1
  -- f j = delayL i 5e-2 (lfdNoise3 j kr (pi/(pi**(exp 1))) * 10e-3 + 10.001e-3)
  f j = delayL i 5e-2 dt where
    dt = linLin (lfdNoise3 j kr (1/8)) (-1) 1 1e-9 35e-3
  i = "a_in"@@0
  a = ("amp"@@1) * fadeOutEnv

mix001 = replaceOut ("out"@@0) o where
  o = limiter o' 1 0.1 * ("mamp"@@1) * fadeOutEnv
  o' = mce [sum ls, sum rs]
  ls = ["a_l1"@@0,"a_l2"@@2, "a_l3"@@4]
  rs = ["a_r1"@@1, "a_r2"@@3,"a_r3"@@5]

fadeOutEnv = envGen kr 1 1 0 1 RemoveSynth $
             env [0,1,1,0] [0,226,5] [EnvCub] (-1) 0

-- Better to add these 2 functions to lepton.
grp = Group
syn = Synth

w = withSC3
prn = printRootNode

-- | Synonym operator of @mceChannel@
(!!*) :: UGen -> Int -> UGen
ug !!* n = mceChannel n ug

infixl 7 !!*
