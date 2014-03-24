{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Sample patterns.
-}
module Scratch.SampleData where

import Control.Monad
import Data.Function (fix)
import System.Random
import System.FilePath

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import System.Random.Mersenne.Pure64
import Sound.SC3.Lepton.Pattern.Parse2

import qualified Data.ByteString.Lazy.Char8 as LC8

import Scratch.L

l = withLept

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("amp", prepeat 0.1)
  ,("freq", midiCPS pspeFreq)]

-- pspeFreq =
--   pcycle
--     [prand 1
--        [pempty, plist [24,31,36,43,48,55]]
--     ,pseq (prange 2 5)
--        [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
--     ,prand (prange 3 9)
--        [74,75,77,79,81]]

pspeFreq =
  pcycle
    [prand (pval 1)
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange (pval 2) (pval 5))
       [ pval 60, prand (pval 1) [pval 63, pval 65]
       , pval 67, prand (pval 1) [pval 70,pval 72,pval 74]]
    ,prand (prange (pval 3) (pval 9))
       [pval 74,pval 75,pval 77,pval 79,pval 81]]

-- Unison, using same random seed.
pspe2 =
  ppar
    [psnew "speSynth" Nothing AddToTail 1
     [("dur", prepeat 0.13)
     ,("amp", prepeat 0.1)
     ,("freq", midiCPS pspeFreq)]
    ,psnew "speSynth" Nothing AddToTail 1
     [("dur", prepeat 0.13)
     ,("amp", prepeat 0.1)
     ,("freq", midiCPS (prepeat (-5) + pspeFreq))]
    ]

-- Non-unison, bundled two patterns.
-- Different random seed will be used in leptseq.
-- pspe2' =
--   bundle' 1 0
--     [ l_new "pspe1" $
--       psnew "speSynth" Nothing AddToTail 1
--       [("dur", prepeat 0.13)
--       ,("amp", prepeat 0.1)
--       ,("freq", midiCPS pspeFreq)]
--     , l_new "pspe2" $
--       psnew "speSynth" Nothing AddToTail 1
--       [("dur", prepeat 0.13)
--       ,("amp", prepeat 0.1)
--       ,("freq", midiCPS (prepeat (-7) + pspeFreq))]
--     ]

-- Unison, using papp and plam
pspe3 =
   (plam
    (\x ->
      ppar
      [psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat 0.13)
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS x)]
      ,psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat 0.13)
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS x * prepeat 0.498)]
      ,psnew "speSynth" Nothing AddToTail 1
       [("dur", prepeat 0.13)
       ,("amp", prepeat 0.1)
       ,("freq", midiCPS x * prepeat 2.002)]
      ]
    )) `papp` pspeFreq

-- Unison, high pitch pattern repeating same note.
pspe4 =
  ppar
  [psnew "speSynth" Nothing AddToTail 1
   [("dur", prepeat 0.13)
   ,("amp", prepeat 0.1)
   ,("freq", (papp (plam (\x -> preplicate 3 (midiCPS x))) pspeFreq))]
  ,psnew "speSynth" Nothing AddToTail 1
   [("dur", prepeat 0.39)
   ,("amp", prepeat 0.1)
   ,("freq", prepeat 0.25 * midiCPS pspeFreq)]
  ]

pspe5 =
  psnew "speSynth" Nothing AddToTail 1
   [("dur", prepeat 0.13)
   ,("amp", prepeat 0.1)
   ,("freq",
     (papp (plam (\x -> midiCPS $ pconcat [x-12,x,x+12,x])) pspeFreq))]

pspe6 =
  ppar
  [psnew "speSynth" Nothing AddToTail 1
   [("dur", prepeat 0.13)
   ,("amp", prepeat 0.1)
   ,("freq", (papp (plam (\x -> preplicate 16 (midiCPS (x-24)))) pspeFreq))]
  ,psnew "speSynth" Nothing AddToTail 1
   [("dur", prepeat 0.13)
   ,("amp", prepeat 0.1)
   ,("freq", (papp (plam (\x -> preplicate 16 (midiCPS (x-17)))) pspeFreq))]
  ]

-- psw'for'180'seconds path =
--   writeScore [] (Group 0 [Group 1 []]) (ptakeT 180 psw) path

psw = pappend set03 (ppar [loop01, loop02, loop03])

-- psw' =
--   bundle' 1 0
--     [ l_new "set03" set03
--     , l_new "loop01" loop01
--     , l_new "loop02" loop02
--     , l_new "loop03" loop03 ]

goLoop01 = audition $ toR loop01

loop01 = psnew "rspdef1" Nothing AddToHead 1
  [("dur",  pcycle [preplicate 1024 (1/41)
                   ,preplicate 512 (2/41)
                   ,preplicate 256 (4/41)
                   ,preplicate 128 (8/41)])
  ,("freq", midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  ,("n_map/fmul", pforever 100)]

loop02 = psnew "rspdef2" Nothing AddToHead 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp $ prange (log 110) (log 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]

p01 = p01' (prange (-1) 1)

p01' pan = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [1, 0.51,0.49])
  ,("freq", pcycle [1980,1540,1320 ,1100,990,880, 660,440,330, 220,110,55])

  -- ,("freq",
  --   prepeat 2 * (midiCPS $ pcycle [60,64,67, 57,60,54, 62,65,69, 59,62,53]))

  ,("amp", pcycle [0.75,1,1.5,2,1,0.75])
  ,("pan", pforever pan)]

p02 pan = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [0.52,0.48,0.51,0.49])
  ,("freq", midiCPS $ pcycle [60,64,67,72,67,64,55,59,62,67,62,59])
  ,("amp", pcycle [0.5,0.75,1,1.25,1,0.75])
  ,("pan", pforever pan)]

------------------------------------------------------------------------------
-- Communicating with pattern server

-- goP01s :: IO ()
-- goP01s = withLept $ \fd -> forM_ [1..10] $ \i -> do
--   pan <- randomRIO (-1,1)
--   send fd =<< bundle' 8 (0.001*2**fromIntegral i)
--     [l_new ("p01_" ++ show i) (p01' (pval pan))]

------------------------------------------------------------------------------
-- Testing behaviour of finite patterns

p03 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 1)
  ,("freq", plist [880,660,440,330,220])
  ,("amp", plist [0.5,0.4,0.5,0.4,0.3])]

------------------------------------------------------------------------------
-- Finite state pattern

pfsm001 = pfsm [0]
  [ (pm1, [0,1,2])
  , (pm2, [0])
  , (pm3, [0,1,2,3])
  , (pm4, []) ]

pm1 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 0.125)
  ,("freq", plist [880,660,440,330,220])
  ,("amp", plist [0.5,0.4,0.5,0.4,0.3])
  ]

pm2 = psnew "rspdef1" Nothing AddToTail 1
  [("dur",
    pforever (prand 1 [plist [0.125,0.125], 0.25, plist [0.375,0.125]]))
  ,("freq", prand 12 [400,330,1320,990,660,880])
  ,("amp", pforever (prange 0.2 0.5))
  ]

pm3 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever (prand 1 [plist [0.125,0.125], 0.25]))
  ,("freq",
    pseq 6 [880, prand 1 [2200,3300,4400,5500,6600,7700,8800]])
  ,("amp", pforever (prange 0.4 0.6))
  ]

pm4 = pempty

pfsm002 = pfsm [0]
  [(mkfsm02 [0,4,7],    [1,2,3,4])
  ,(mkfsm02 [2,5,9],    [3])
  ,(mkfsm02 [0,4,9],    [0,1,3,4])
  ,(mkfsm02 [2,5,7,-1], [0,4])
  ,(mkfsm02 [0,4,9],    [1,2,3])
  ]

mkfsm02 fs =
  let fs' = concatMap (\x -> map pval [{-x+60,-}x+72{-,x+84-}]) fs in
  psnew "rspdef1" Nothing AddToTail 1
    [("dur",  pforever (prand 1 [plist [0.125,0.125],0.25]))
    ,("freq", midiCPS $ pforever (prand 1 fs'))
    ,("atk",  pforever $ prange 1e-4 3e-2)
    ,("dcy",  pforever $ prange 1e-1 1)
    ,("pan",  pforever $ prand 1 [-1,-0.5,0,0.5,1])
    ,("amp",  preplicate 16 (prange 0.4 0.6))]

p003 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 0.5 {- (prand 1 [plist [0.5,0.5], plist [1.5,0.5], 1]) -})
  ,("freq", midiCPS $ pforever (plist [36,48]))
  ,("atk", pforever $ prange 2e-2 3e-2)
  ,("dcy", pforever $ prange 1 2)
  ,("pan", prepeat 0.1)
  ,("amp", pforever (prange 0.3 0.5))]


------------------------------------------------------------------------------
-- Pausable patterns

pp00t = 0.1333

pp001 = psnew "rspdef1" Nothing AddToTail 1
  [("dur",
    pforever (prand 1 [pp00t*2, plist [pp00t,pp00t], plist [pp00t*3, pp00t]]))
  ,("freq", midiCPS $ pforever (prand 1 [72,75,77,79,82, 84,87,89,91,94]))
  ,("amp", pforever (prange 0.3 0.5))
  ,("atk", pforever (prange 1e-4 1e-2))
  ,("dcy", pforever (prange 1e-2 1))
  ,("pan", prepeat (-0.3))]

pp002 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", prepeat (2*pp00t))
  ,("freq",
    midiCPS $ pforever $
    pconcat [ pseq 3 [48,55,60,55,60,55]
            , plist [60,67,72,67,72,67] ])
  ,("amp", pforever (prange 0.3 0.5))
  ,("atk", pforever (prange (1e-4) (3e-4)))
  ,("dcy", pforever (prange 1 2))
  ,("pan", pforever (prand 1 [-0.75,0.75]))]

pp003 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [preplicate 4 (pval pp00t), pval pp00t*2])
  ,("freq", midiCPS $ pforever $
            pconcat [ plist [72,82,79,75,77]
                    , plist [84,87,89,91,94]
                    , plist [96,99,103,106,101]
                    , plist [84,94,91,89,87]
                    ])
  ,("amp", pforever (prange 0.3 0.5))
  ,("atk", pforever (prange 1e-4 1e-2))
  ,("pan", prepeat 0.3)
  ,("dcy", pforever (prange 1e-2 1))]

-- add'pp001 = leptseq =<< bundle' (pp00t*2) 0 [l_new "pp001" pp001]
-- add'pp002 = leptseq =<< bundle' (pp00t*48) 0 [l_new "pp002" pp002]
-- add'pp003 = leptseq =<< bundle' (pp00t*48) 0 [l_new "pp003" pp003]

-- del'pat name = leptseq =<< bundle' (pp00t*2) 0 [l_free name]
-- pause'at i name = leptseq =<< bundle' (pp00t*i) 0 [l_pause name]
-- run'at i name = leptseq =<< bundle' (pp00t*i) 0 [l_run name]

es01 = pseq (prange 1 4) [1, plist [2,3,4], prange 5 10]

pshared01 s =
  let p1 = runP es01 g
      p2 = runP (es01 + prepeat 10) g
      g = pureMT s
  in  zip p1 p2

------------------------------------------------------------------------------
-- Lambda and app

pla01 = papp (plam (\x -> pconcat [x,x]))
        (psnew "rspdef1" Nothing AddToTail 1
         [("dur", pforever (prange 0.125 2))
         ,("freq",prepeat 440)
         ,("amp", pforever (prange 1e-1 3e-1))])

pla02 =
  psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever (prand 1 [preplicate 4 0.125,preplicate 2 0.25,0.5]))
  ,("freq", pforever (papp (plam (\x -> pconcat [x,440,x*2,660]))
                      (prand 1 [110,220,330,550,770,880,990])))
  ,("amp", pforever (prange 3e-1 5e-1))]

pla03 =
  psnew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 0.13)
  ,("freq", pforever (papp (plam (\x -> pconcat [x,x*2])) (prange 100 200)))
  ,("amp", prepeat 0.3)]

pmap = papp . plam

nla04 =
  Group 0
    [ Group 1 [Synth 2001 "otfrev2" []]]

pla04 =
  psnew "smpld01" Nothing AddToHead 1
  [("dur", pforever 0.13)
  ,("dcy", pforever (log $ prange (exp 1e-1) (exp 0.5)))
  ,("freq",
    pforever (pmap (\x ->
               midiCPS $ prand 1 [pconcat [x,x+4,x+7,x+12,x+7,x+4]
                                 ,pconcat [x-5,x,x+4,x+7,x+4,x]
                                 ,pconcat [x-8,x-5,x,x+4,x,x-5]])
                      (prand 1 [60,61..72])))
  ,("amp", pforever (prange 0.2 0.5))]

pla05 =
  plam (\x ->
         let dt = 0.217 in
         psnew "smpld01" Nothing AddToHead 1
         [-- ("dur", prand 1 [preplicate 4 dt,preplicate 2 (dt*2)])
          ("dur", dt)
         ,("dcy", (1024/(x*x)))
         ,("atk", (exp $ prange (log 1e-3) (log 1)))
         -- ,("freq", midiCPS (prand 1 [{-x-24,-}x{-,x+12-}]))
         ,("freq", midiCPS (prand 1 [x-24,x,x+12]))
         ,("pan", prange (-1) 1)
         ,("amp", (prange 0.05 0.5))])
  `papp`
  let scale = [60,65,67,70,66,63,72] in
  pforever (pseq (pval 8) [pseq 1 scale, prand 1 scale])
  -- (pforever (prand 1 [60,63,65,66,67,70,72]))

smpld01 = out ("out"@@0) (pan2 sig ("pan"@@0) 1) where
  sig =
    sum [fSinOsc AR frq 0 * e 1 rs
        ,fSinOsc AR (frq*1.98) (0.32*pi) * 0.2 * e 0.8 dn
        ,fSinOsc AR (frq*3.02) (0.93*pi) * 0.18 * e 0.7 dn
        ,fSinOsc AR (frq*3.998) (0.171*pi) * 0.12 * e 0.6 dn
        ,fSinOsc AR (frq*4.9987) (0.162*pi) * 0.4 * e 0.3 dn
        ,fSinOsc AR (frq*6.0017) (0.591*pi) * 0.3 * e 0.42 dn
        ]
  dn = DoNothing; rs = RemoveSynth
  frq = ("freq"@@440)
  e d a =
    envGen KR 1 ("amp"@@0.3) 0 d a $
    env [0,1,0] ["atk"@@1e-4,"dcy"@@1e-4] [EnvSin] (-1) 0

------------------------------------------------------------------------------
-- L

pspeL =
  let dt = 0.39 in
  lam (
    ppar
    [psnew "speSynth" Nothing AddToTail 1
      [("dur", prepeat (dt/3))
      ,("amp", prepeat 0.1)
      ,("freq", preplicate 3 (midiCPS pz))]
    ,psnew "speSynth" Nothing AddToTail 1
      [("dur", prepeat dt)
      ,("amp", prepeat 0.1)
      ,("freq", midiCPS (pz-12))]
    ]) `app` pspeFreq

------------------------------------------------------------------------------
-- Checking behaviour of range

lll02 = let d = pval in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur", pforever (d 0.4))
  ,("freq", pforever (d 880))
  ,("atk", pforever (prange (d 0.001) (d 1)))]


lll01 = let d = pval in
  psnew "rspdef1" Nothing AddToHead 1
  [("dur", pforever (d 0.2))
  ,("amp", pforever (d 0.3))
  ,("pan", pforever (prange (d (-1)) (d 1)))
  ,("freq", pforever $ prange (d 100) (d 8000))
  ,("atk", pforever (d 1e-4))
  ,("dcy", pforever (d 1))
  ]
