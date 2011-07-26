{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Module to play with sending node repeatedly with response to sendReply
message. In each 64 beat, sending node that contains demand rate
triggering ugen to hit percussive synths.

-}
module Cd22e9d.Nodes where

import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

main = w respond

go = w $ \fd -> do
  send fd $ c_set [(1001,1)]
  patchNode n0 fd

n0 =
  Group 0
  [Group 1
   [Group 10 -- master control
    [Synth 1000 "met9d"
     ["bpm":=295,"out":=100]
    ,Synth 1001 "tr9d"
     ["t_trig":<-100,"out":=99]
    ,Synth 1002 "fo9d"
     ["t_barCount":<-100, "out":=1001]
    ]
   ,n11      -- control
   ,n20      -- source
   ,n21      -- effects
   ,n90      -- mix
   ,Group 99 -- master mix
     [Synth 9999 "cd2mst"
      ["out":=0,"amp":=1]]]
  ]

-- controls, for recording
n11 =
  Group 11
  [Synth 1101 "cd2thuh1"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2that"
   ["out":=102,"t_trig":<-100]
  ,Synth 1103 "cd2tsnr"
   ["out":=103,"t_trig":<-100]
  ,Synth 1104 "cd2tkik"
   ["out":=104,"t_trig":<-100]
  ,Synth 1105 "cd2tdrn1"
   ["outg":=105,"outf":=106,"t_trig":<-100]
  ,Synth 11051 "cd2tdrn2"
   ["outg":=109,"outf":=110,"t_trig":<-100]
  ,Synth 1106 "cd2thuh2"
   ["out":=107,"t_trig":<-100]
  ,Synth 1107 "cd2thuh3"
   ["out":=108,"t_trig":<-100]
  ,Synth 1108 "cd2tpua"
   ["out":=111,"t_trig":<-100]
  ]

-- controls, variant a
n11a =
  Group 11
  [Synth 1101 "cd2thuh1a"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2thata"
   ["out":=102,"t_trig":<-100]
  ,Synth 1103 "cd2tsnra"
   ["out":=103,"t_trig":<-100]
  ,Synth 1104 "cd2tkika"
   ["out":=104,"t_trig":<-100]
  ,Synth 1105 "cd2tdrn1a"
   ["outg":=105,"outf":=106,"t_trig":<-100]
  ,Synth 11051 "cd2tdrn2a"
   ["outg":=109,"outf":=110,"t_trig":<-100]
  ,Synth 1106 "cd2thuh2a"
   ["out":=107,"t_trig":<-100]
  ,Synth 1107 "cd2thuh3a"
   ["out":=108,"t_trig":<-100]
  ,Synth 1108 "cd2tpua"
   ["out":=111,"t_trig":<-100]
  ]

-- controls, variant b
n11b =
  Group 11
  [Synth 1101 "cd2thuh1b"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2thatb"
   ["out":=102,"t_trig":<-100]
  ,Synth 1103 "cd2tsnrb"
   ["out":=103,"t_trig":<-100]
  ,Synth 1104 "cd2tkikb"
   ["out":=104,"t_trig":<-100]
  ,Synth 1105 "cd2tdrn1b"
   ["outg":=105,"outf":=106,"t_trig":<-100]
  ,Synth 11051 "cd2tdrn2b"
   ["outg":=109,"outf":=110,"t_trig":<-100]
  ,Synth 1108 "cd2tpua"
   ["out":=111,"t_trig":<-100]
  ]

-- source
n20 =
  Group 20
  [Synth 2001 "cd2huh"
   ["out":=10,"t_trig":<-101]
  ,Synth 2002 "cd2hat"
   ["out":=11,"t_trig":<-102]
  ,Synth 2003 "cd2snr"
   ["out":=12,"t_trig":<-103]
  ,Synth 2004 "cd2kik"
   ["out":=13,"t_trig":<-104]
  ,Synth 2005 "cd2pu"
   ["out":=17,"t_trig":<-100,"amp":=0.3,"freq":<-111]
  ,Synth 2006 "cd2drn"
   ["out":=14,"gate":<-105,"freq":<-106]
  ,Synth 20061 "cd2drn"
   ["out":=19,"gate":<-109,"freq":<-110]
  ,Synth 2007 "cd2huh"
   ["out":=15,"t_trig":<-107]
  ,Synth 2008 "cd2huh"
   ["out":=16,"t_trig":<-108]
  ]

-- effects
n21 =
  Group 21
  [Synth 2101 "cd2rev"
   ["out":=10,"a_in":<=10,"dlyt":=0.01,"dmul":=0.008]
  ,Synth 2102 "cd2rev"
   ["out":=12,"a_in":<=12,"dlyt":=0.02,"dmul":=0.008]
  ]

-- mix
n90 =
  Group 90
  [Synth 9001 "cd2mix" -- huh
   ["out":=0,"a_in":<=10,"amp":=1.4,"pan":=0.0]
  ,Synth 9002 "cd2mix" -- hat
   ["out":=0,"a_in":<=11,"amp":=0.1,"mamp":<-1001,"pan":=(-0.2)]
  ,Synth 9003 "cd2mix" -- snr
   ["out":=0,"a_in":<=12,"amp":=0.4,"mamp":<-1001,"pan":=(-0.1)]
  ,Synth 9004 "cd2mix" -- kik
   ["out":=0,"a_in":<=13,"amp":=0.9,"pan":=0.03]
  ,Synth 9005 "cd2mix" -- drn
   ["out":=0,"a_in":<=14,"amp":=0.5,"mamp":<-1001,"pan":=(-0.15)]
  ,Synth 90051 "cd2mix" -- drn b
   ["out":=0,"a_in":<=19,"amp":=0.5,"mamp":<-1001,"pan":=(0.15)]
  ,Synth 9006 "cd2mix" -- huh2
   ["out":=0,"a_in":<=15,"amp":=0.6,"pan":=(-0.8)]
  ,Synth 9007 "cd2mix" -- huh3
   ["out":=0,"a_in":<=16,"amp":=0.6,"pan":=0.8]
  ,Synth 9008 "cd2mixm" -- pu right
   ["out":=0,"a_in":<=17,"mamp":<-1001,"amp":=1]
  ,Synth 9009 "cd2mixm" -- pu left
   ["out":=1,"a_in":<=18,"mamp":<-1001,"amp":=1]
  ]

w = withSC3

respond :: Transport t => t -> IO b
respond fd = do
  async fd (notify True)
  forever $ wait fd "/cd22e9d" >>= flip work fd

work :: Transport t => OSC -> t -> IO ()
work (Message "/cd22e9d" [Int _,Int _,Float v]) =
  let n = ceiling v :: Int
      (q,r) = n `quotRem` 64
  in  case r of
    0 -> case q of
      q | q `mod` 4 == 1 -> patchNode n11b
        | otherwise      -> patchNode n11a
    _ -> const $ return ()
