{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Rewrite of Cd22e9d synth and events, with using patterns and response based
sequence codes.

-}
module Cf22e9d where

import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton hiding (s)

import Respond hiding (setup)
import qualified Respond as R
import qualified RespTest02 as RT02

main :: IO ()
main = w $ \fd ->
  setup fd >> patchNode n0 fd >> runMsg allP fd

setup :: Transport t => t -> IO OSC
setup fd = do
  R.setup fd
  async fd $ bundle immediately $
    map (d_recv . uncurry synthdef)
      [("cf2huh", cf2huh)
      ,("cf2nzf", cf2nzf)
      ,("cf2kik", cf2kik)
      ,("cf2snr", cf2snr)
      ,("cf2hat", cf2hat)
      ,("cf2drn", cf2drn)
      ,("cf2pu", cf2pu)
      ,("cf2bell", cf2bell)
      ,("cf2shw", cf2shw)
      ,("cf2rev", cf2rev)
      ,("cf2dly", cf2dly)
      ,("cf2mix", cf2mix)
      ,("cf2mixm", cf2mixm)
      ,("cf2mst", cf2mst)
      ]

n0 :: SCNode
n0 =
  g 0
    [g 1
     [g 10
      [s 1000 "lfsin" []
      ,s 1001 "cf2drn"
      ["out":=19,"gate":=1]
      ,s 1002 "cf2drn"
      ["out":=20,"gate":=1]]
     ]
    ,g 2
     [s 2000 "cf2rev" -- huh1
      ["out":=10,"a_in":<=10,"dlyt":=0.01,"dmul":=0.008]
     ,s 2001 "cf2rev" -- snr
      ["out":=14,"a_in":<=14,"dlyt":=0.02,"dmul":=0.008]
     ,s 2002 "cf2dly" -- bell
      ["out":=18,"a_in":<=18,"maxdt":=0.8]
     ]
    ,g 8
     [s 8000 "cf2mix" -- huh1
      ["out":=0,"a_in":<=10,"amp":=1.4,"pan":=0]
     ,s 8001 "cf2mix" -- huh2
      ["out":=0,"a_in":<=11,"amp":=1.2,"pan":=(-0.8)]
     ,s 8002 "cf2mix" -- huh3
      ["out":=0,"a_in":<=12,"amp":=1.2,"pan":=0.8]
     ,s 8003 "cf2mix" -- kik
      ["out":=0,"a_in":<=13,"amp":=0.8,"pan":=0.03]
     ,s 8004 "cf2mix" -- snr
      ["out":=0,"a_in":<=14,"amp":=0.35,"pan":=(-0.1)]
     ,s 8005 "cf2mix" -- hat
      ["out":=0,"a_in":<=15,"amp":=0.1,"pan":=(-0.2)]
     ,s 8006 "cf2mixm" -- pu right
      ["out":=0,"a_in":<=16,"amp":=1]
     ,s 8007 "cf2mixm" -- pu left
      ["out":=1,"a_in":<=17,"amp":=1]
     ,s 8008 "cf2mix"  -- bell
      ["out":=0,"a_in":<=18,"amp":=0.8,"pan":=0.1]
     ,s 8009 "cf2mix" -- drn 1
      ["out":=0,"a_in":<=19,"amp":=0.5,"pan":=(-0.15)]
     ,s 8010 "cf2mix" -- drn 2
      ["out":=0,"a_in":<=20,"amp":=0.5,"pan":=0.15]
     ]
    ,g 9
     [s 9000 "cf2mst"
      ["out_l":=0, "out_r":=1, "amp":=1]]]
  where
    g = Group
    s = Synth

--
-- Patterns
--

bpm = 295

allP = ppar
  [ huh1P{-, huh2P, huh3P -}
  , kikP, snrP, hatP
  , puP, drn1P, drn2P, bellP
  ]

-- allP = ppar [ huh1P, huh2P, huh3P
--             , kikP, snrP, hatP
--             , bellP
--             , RT02.loop01
--             , RT02.loop02
--             , RT02.loop03 ]

g10 =
  g 0
  [g 1
   [g 10 []]
  ,g 8
   [s 8000 "cf2mix" -- huh1
    ["out":=0,"a_in":<=10,"amp":=1.4,"pan":=0]
   ,s 8001 "cf2mix" -- huh2
    ["out":=0,"a_in":<=11,"amp":=1.2,"pan":=(-0.8)]
   ,s 8002 "cf2mix" -- huh3
    ["out":=0,"a_in":<=12,"amp":=1.2,"pan":=0.8]
   ,s 8003 "cf2mix" -- kik
    ["out":=0,"a_in":<=13,"amp":=0.8,"pan":=0.03]
   ]]
  where
    g = Group
    s = Synth

testHuh :: IO ()
testHuh = w $ \fd -> do
  reset fd
  patchNode g10 fd
  runMsg p fd
  where
    -- p = huh1P
    -- p = ppar [huh1P, kikP, puP]
    p = ppar [huh1P,huh2P,huh3P]

huh1P =
  mkSnew AddToTail 10 "cf2huh"
  [("dur", pforever (60/bpm))
  ,("out", pforever 10)
  ,("t_trig",
    pseq 1
    [pseq 4 [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0]
    ,pcycle
     [pseq 12 [0,1,0,0, 1,0,0,1, 0,0,1,0, 1,0,1,0]
     ,pseq 4  [0,1,0,1, 1,1,0,0, 0,0,0,1, 1,0,0,1]]])
  ]

huh2P =
  mkSnew AddToTail 10 "cf2huh"
  [("dur", pforever (60/bpm))
  ,("out", pforever 11)
  ,("t_trig",
    pseq 1
    [pseq 16 [0,0,0,0]
    ,pcycle
     [pseq 12 [1,0,1,0, 1,0,0,0, 1,0,1,0, 1,0,0,0]
     ,pseq 14 [0,0,0,0], pseq 4 [0,1]]])
  ]

huh3P =
  mkSnew AddToTail 10 "cf2huh"
  [("dur", pforever (60/bpm))
  ,("out", pforever 12)
  ,("t_trig",
    pseq 1
    [pseq 16 [0,0,0,0]
    ,pcycle
     [pseq 12 [0,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,1]
     ,pseq 14 [0,0,0,0], pseq 4 [1,0]]])
   ]

kikP =
  mkSnew AddToTail 10 "cf2kik"
  [("dur", pforever (60/bpm))
  ,("out", pforever 13)
  ,("t_trig",
    pseq 1
    [pseq 4
     [1,0,0,0, 1,0,0,0, 0.8,0,0,0, 1,0,0,0]
    ,pcycle
     [pseq 3 [pseq 4
              [1,  0,0,0, 0.8,0,0, prand 1 [0,0.7,0.8,1]
              ,0.9,0,0,0, 1,  0,0, prand 1 [0,0.7,0.8,1]]]
     ,pseq 4
      [1,0,0,0.7, 1,0,0,1, 0,0.9,0,0.8, 0.9,0,0,1]]])
   ]

snrP =
  mkSnew AddToTail 10 "cf2snr"
  [("dur", pforever (60/bpm))
  ,("out", pforever 14)
  ,("t_trig",
    pseq 1
    [pseq 56 [0], pseq 1 [0.8,0.6,0,0.2, 0.2,0.8,0.4,1.0]
    ,pcycle
     [pseq 3
      [pseq 3 [pseq 3 [0,0,prange 0.6 1, 0]
              ,pseq 1 [0,0,prange 0.6 0.8, prange 0.6 0.8]]
      ,pseq 1 [pseq 2 [0,1], prand 8 [0,0.5,0.75,1]]]
     ,pseq 3 [0,0,0,0, prand 1 [0.9,1.0],0,0,0
             ,0,0,0,0, prand 4 [1,0.8,0]
             ]
     ,pseq 1 [0,0,0,0, prand 1 [0.9,1.0],0,0,0
             ,prand 8 [0,0,0,0.5,0.6,0.7,0.8,0.9,1]]]])
   ]

hatP =
  mkSnew AddToTail 10 "cf2hat"
  [("dur", pforever (60/bpm))
  ,("out", pforever 15)
  ,("t_trig",
    pseq 1
    [pseq 62 [0], pseq 1 [0.6,0.8]
    ,pcycle
     [pseq 3
      [pseq 32 [prand 1 [0,0,0,0,0,0,0.2]
               ,prand 1 [0.5,0.8,1.0]]]
     ,pseq 32 [0]
     ,pseq 1
      [pseq 28 [0], prand 4 [0,0.3,0.6,0.8,1]]]])
  ]

puP =
  mkSnew AddToTail 10 "cf2pu"
  [("dur", pforever (60/bpm))
  ,("out", pforever 16)
  ,("t_trig", pforever 1)
  ,("freq", fmap midiCPS $
   pcycle
   [prand 7
    [pseq 1 [36,55,62,36, 55,62,36,55]
    ,pseq 1 [36,60,72,36, 60,72,36,60]
    ,pseq 1 [36,53,58,36, 53,58,36,53]]
   ,36, prand 2 [60,67]
   ,36, prand 2 [67,72]
   ,prand 2 [48,53,55,60,65,67]])
  ]

drn1P =
  mkNset 1001
  [("dur", pforever (60/bpm))
  ,("freq", fmap (\x -> if x == 0 then nan else midiCPS x) $
    pseq 1
    [pseq 32 [0]
    ,pcycle
     [pseq 3
      [72, 0, 0, 0,  0, 0, 0, 0,  0,67, 0, 0, 65, 0, 0, 0
      ,67, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0, 65, 0, 0, 0
      ,60, 0, 0, 0,  0, 0, 0, 0,  0,55, 0, 0, 65, 0, 0, 0
      ,67, pseq 15 [0]]
     ,pseq 1
      [72, pseq 31 [0]
      ,60, pseq 31 [0]]]])
  ]

drn2P =
  mkNset 1002
  [("dur", pforever (60/bpm))
  ,("freq", fmap (\x -> if x == 0 then nan else midiCPS x) $
   pseq 1
    [pseq 32 [0]
    ,pcycle
     [pseq 3
      [ 0, 0, 55,0,  0, 0,60, 0,  0, 0, 0, 0,  0, 0, 0, 0
      , 0, 0, 0, 0, 67, 0, 0, 0,  0, 0, 0, 0, 60, 0, 0, 0
      , 0, 0, 0, 0,  0, 0,67, 0,  0, 0, 0, 0,  0, 0, 0, 0
      , 0, 0, 0, 0, 60, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0]
     ,pseq 1
      [ 0, 0, 55,0, pseq 28 [0]
      , 0, 0,  0,0, 0, 0, 67, 0, pseq 24 [0]]]])
   ]

bellP =
  mkSnew AddToTail 10 "cf2bell"
  [("dur", pforever (60/bpm))
  ,("out", pforever 18)
  ,("t_trig", pforever 1)
  ,("freq", fmap midiCPS $
    pforever $ pseq 1
    [pseq 64 [0]
    ,pseq 24 [0]
    ,pcycle
     [prand 16 (map pval $ replicate 16 0 ++ [79,84,89,91,96])
     ,pseq 48 [0]]])
  ]

shwP =
  undefined

--
-- Control synths
--

lfsin :: UGen
lfsin =
  out ("out"@@100) $ sinOsc kr ("freq"@@1) 0 * ("mul"@@1) + ("add"@@0)

--
-- Source synths
--
-- Many synths are using dummy line ugen, to free the node after
-- certain seconds. When synths can free itself with envGen, this dummy
-- line ugen could be removed.
--
-- Using more memory than demand ugen version, need to increase
-- scsynth server memory with '-m' option.
--

-- | Synthdef for 'huh' human vowel like noise.
cf2huh :: UGen
cf2huh = cf2huh' (whiteNoise 'a' ar) ("t_trig"@@0)
cf2huh' srcn tick =
  mrg [out ("out"@@0) (mix $ resonz srcn freq bndw * ampe), d] where
    freq = mce [tRand 'a' 600 800 tick
               ,tRand 'b' 1000 1400 tick
               ,tRand 'c' 2400 2800 tick] `lag` 0.1
    bndw = mce [130/700, 70/1220, 160/2600]
    ampe = envGen kr (coinGate 'g' 0.99 tick) ampv 0 1 RemoveSynth $
           envSine ed ev
    ampv = latch tick tick
    ed = tRand 'd' 0.1 0.4 tick
    ev = tRand 'e' 0.1 0.6 tick
    d = line KR 0 0 1 RemoveSynth

cf2nzf :: UGen
cf2nzf = cf2nzf' ("t_amp"@@0) ("freq"@@0)
cf2nzf' amp freq = out ("out"@@0) sig where
  sig = sum [rlpf nz freq 2 * ae1
            ,rlpf nz (freq*2.002321) 1.5 * ae2
            ,rlpf nz (freq*2.9989989) 1 * ae3]
  nz = pulse ar freq (lfdNoise3 'f' kr 9.32 * 0.5 + 0.5)
  ae1 = mkAE [0,1,0.2,0.8,0] [28e-3,200e-3,100e-3,285e-3]
  ae2 = mkAE [0,0.5,0.8,0] [120e-3,30e-3, 130e-3]
  ae3 = mkAE [0,1,0.2,0] [25e-3, 180e-3, 310e-3]
  mkAE vs ts = envGen kr amp amp 0 0.25 RemoveSynth $
               env vs ts [EnvNum 3] (-1) (-1)

cf2kik :: UGen
cf2kik = cf2kik' ("t_trig"@@0)
cf2kik' tick =
  out ("out"@@0) ((lfCub ar freq 0.05 + impl) * ampe) where
    freq = (mix $ mce [200.32, 230.32, 360.79, 110.13]) * fenv
    fenv = envGen kr tick 1 0 1 DoNothing $
           env [0.2, 0.2, 0.1, 0.1] [10e-3, 10e-3, 10e-3] [EnvSqr] 0 (-1)
    impl = impulse ar 28 0.3 * decay2 tick 1e-4 2e-3
    ampe = envGen kr tick lvls 0 1 RemoveSynth $
           env [0,1,1,0] [1e-3,25e-3,228e-3] [EnvNum (-13)] (-1) 0
    lvls = latch tick tick * d
    d = line KR 1 1 1 RemoveSynth

cf2snr :: UGen
cf2snr = cf2snr' ("t_trig"@@0)
cf2snr' tick = out ("out"@@0) (sig * ampe * 0.3) where
  sig = mix $ ringz (whiteNoise 'a' ar) fs qs * 0.1
  fs = mce [943.232, 350.32, 680.192]
  qs = mce [0.1,0.05,0.025]
  ampe = envGen kr tick amp' 0 1 RemoveSynth $
         env [0,1,1,0] [1e-3,15e-3,189e-3] [EnvNum (-8)] (-1) 0
  amp' = latch tick tick * d
  d = line KR 1 1 1 RemoveSynth

cf2hat :: UGen
cf2hat = cf2hat' ("t_trig"@@0)
cf2hat' tick = out ("out"@@0) (sig * amp) where
  sig = mix $ rhpf (whiteNoise 'z' ar)
        (mce [5908.32,8803,6723]) (mce [0.1,0.1,0.1])
  amp = envGen kr tick tamp 0 1 RemoveSynth $
        env [0,1,0] [3e-4, 80e-3] [EnvNum (-3)] (-1) (-1)
  tamp = latch tick tick * d
  d = line KR 1 1 1 RemoveSynth

cf2drn :: UGen
cf2drn = cf2drn' ("amp"@@0.3) ("gate"@@1)
         ((("freq"@@440) `lag` 0.6 `lag` 0.6))
cf2drn' amp gt freq = out ("out"@@0) sig where
  sig = foldr (\a b -> allpassN b 0.05 a 4) sig' $
        map (\i -> mce2 (rand i 0 0.05) (rand (succ i) 0 0.05)) "qrst"
  sig' = resonz sig'' q 0.5
  q = lfdNoise3 '\813' kr 1.23 * 2300 + 3000
  sig'' = lfPulse ar freq 0 bw * aenv
  bw = lfdNoise3 '\992' kr 0.7778 * 0.4 + 0.5
  aenv = envGen kr gt amp 0 1 DoNothing $
         env [0, 1, 0.8, 0.8, 0]
         [5e-3, 20e-3, 20e-3, 30e-3] [EnvCub] 2 1

cf2pu :: UGen
cf2pu = cf2pu' ("t_trig"@@0)
cf2pu' tick =
  out ("out"@@0) $ foldr f v (zipWith mce2 rs1 rs2) where
    v = rlpf (pulse ar (mce2 freq (freq*1.01)) bw * 0.2 * ampe * amp)
        (lfdNoise3 'n' kr 2.323 * 2000 + 2200)
        (lfdNoise3 'q' kr 1.110 * 0.498 + 0.5)
    f a b = allpassN b 0.05 a 4
    rs1 = map mkR "abcd"
    rs2 = map mkR "efgh"
    mkR x = rand x 0.001 0.05
    freq = "freq"@@0 `lag` 0.6
    ampe = decay2 tick 5e-4 950e-3
    amp = "amp"@@0.3 * d
    bw = lfdNoise3 'b' kr 0.1123 * 0.48 + 0.5
    d = line KR 1 1 2 RemoveSynth

cf2bell :: UGen
cf2bell = cf2bell' ("t_trig"@@0) ("freq"@@0) ("amp"@@0.3)
cf2bell' tick freq amp = out ("out"@@0) (sig * amp) where
  sig = foldr f sig' (zipWith mce2 rs1 rs2)
  sig' = sinOsc ar (mce [freq',freq'*1.01]) 0 * aenv
  freq' = freq''
  freq'' = latch freq tick
  f a b = allpassL b 250e-3 a 2
  rs1 = map mkR [(1::Int)..8]
  rs2 = map mkR [(1001::Int)..1008]
  mkR x = expRand x 1e-4 150e-3
  nz = pinkNoise 'p' ar
  aenv = decay2 tick 50e-3 1 * d
  d = line KR 1 1 2 RemoveSynth

cf2shw :: UGen
cf2shw = cf2shw' ("t_trig"@@0)
cf2shw' tick = out ("out"@@0) (resonz sig freq bw) where
  sig = brownNoise 'b' ar * tamp
  tamp = decay2 tick 1e-3 1
  freq = el * ("freq"@@8000) + 50
  bw = el * 0.8 + 0.1
  el = envGen kr ("t_envr"@@0) 1 0 1 RemoveSynth $
       env [2.5e-2,1,2.5e-2] [25e-3,8.4] [EnvCub] (-1) (-1)

--
-- Effect synths
--

cf2rev :: UGen
cf2rev = cf2rev' ("a_in"@@0)
cf2rev' input = replaceOut ("out"@@0) sig where
  sig = mix $ foldr f input (zipWith mce2 r1 r2)
  r1 = map mkR [(1::Int) .. 3]
  r2 = map mkR [(101::Int) .. 103]
  mkR i = rand i 0.001 0.05
  f a b = b + combC b 0.5 ("dlyt"@@0.123) a * ("dmul"@@0.25)

cf2dly :: UGen
cf2dly = cf2dly' ("a_in"@@0)
cf2dly' input = replaceOut ("out"@@0) sig where
  sig = foldr f input rs
  f a b = delayL b ("maxdt"@@0.5) a
  rs = map mkR "asdqwerpoiu;lkj/.m"
  mkR x = expRand x 5e-3 ("maxdt"@@0.5)

cf2mix :: UGen
cf2mix = cf2mix' ("a_in"@@0)
cf2mix' input =
  out ("out"@@0) ((pan2 input ("pan"@@0) 1) * ("amp"@@0) * ("mamp"@@1))

cf2mixm :: UGen
cf2mixm = cf2mixm' ("a_in"@@0)
cf2mixm' input = out ("out"@@0) (input * ("amp"@@1))

cf2mst :: UGen
cf2mst = cf2mst' ("amp"@@0)
cf2mst' amp = mrg [l', r'] where
  l' = replaceOut ("out_l"@@0) (hpf l 15 * amp)
  r' = replaceOut ("out_r"@@1) (hpf r 15 * amp)
  -- (hpf (limiter (mce [l,r] * amp) 1 0.25) 15) where
  l = in' 1 ar 0
  r = in' 1 ar 1
