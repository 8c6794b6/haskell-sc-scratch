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

import Data.Binary (encode, decode)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.Expr
import Sound.SC3.Lepton.Pattern.Play

import Sound.SC3.Lepton.Pattern.Client
import Sound.SC3.Lepton.Pattern.ParseP (parseP)

import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as LC8

l = withLept

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

pspeFreq =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

psw = pappend set03 (ppar [loop01, loop02, loop03])

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

goP01s :: IO ()
goP01s = withLept $ \fd -> forM_ [1..10] $ \i -> do
  pan <- randomRIO (-1,1)
  send fd =<< bundle' 8 (0.001*2**fromIntegral i)
    [l_new ("p01_" ++ show i) (p01' (pval pan))]

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
  [(mkfsm02 [0,4,7,11], [0,1,2,3,4])
  ,(mkfsm02 [2,5,9,12], [1,3])
  ,(mkfsm02 [0,4,5,9],  [2,0,1,3,4])
  ,(mkfsm02 [2,5,7,-1], [3,4,0])
  ,(mkfsm02 [0,4,7,9],  [4,1,2,3,4])
  ]

mkfsm02 fs = let fs' = concatMap (\x -> map pval [x+60,x+72,x+84]) fs in
  psnew "rspdef1" Nothing AddToTail 1
    [("dur",  pforever (prand 1 [plist [0.125,0.125],0.25]))
    ,("freq", midiCPS $ pforever (prand 1 fs'))
    ,("atk",  pforever $ prange 1e-3 0.5)
    ,("dcy",  pforever $ prange 1e-2 1)
    ,("pan",  pforever $ prand 1 [-1,-0.5,0,0.5,1])
    ,("amp",  preplicate 16 (prange 0.4 0.6))]