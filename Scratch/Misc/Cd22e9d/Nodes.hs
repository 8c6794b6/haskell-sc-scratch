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

go = w . patchNode $ n0

n0 =
  Group 0
  [Group 1
   [Group 10 -- master control
    [Synth 1000 "met9d"
     ["bpm":=295,"out":=100]
    ,Synth 1001 "tr9d"
     ["t_trig":<-100]]
   ,n11a -- control
   ,n20 -- source 
   ,n21 -- effects
   ,n90
   ,Group 99 -- master
     [Synth 9999 "cd2mst"
      ["out":=0,"amp":=1]]]]

-- controls, variant a
n11a =
  Group 11
  [Synth 1101 "cd2thuh"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2that"
   ["out":=102,"t_trig":<-100]
  ,Synth 1103 "cd2tsnr"
   ["out":=103,"t_trig":<-100]
  ,Synth 1104 "cd2tkik"
   ["out":=104,"t_trig":<-100]
  ,Synth 1105 "cd2tnzfa"
   ["outa":=105,"outf":=106,"t_trig":<-100]
  ,Synth 1106 "cd2tnzfb"
   ["outa":=107,"outf":=108,"t_trig":<-100]
  ,Synth 1107 "cd2tdrn"
   ["outg":=109,"outf":=110,"t_trig":<-100]]
  
-- controls, variant b
n11b =
  Group 11
  [Synth 1101 "cd2thuh2"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2that2"
   ["out":=102,"t_trig":<-100]
  ,Synth 1103 "cd2tsnr2"
   ["out":=103,"t_trig":<-100]
  ,Synth 1104 "cd2tkik2"
   ["out":=104,"t_trig":<-100]
  ,Synth 1105 "cd2tnzfa2"
   ["outa":=105,"outf":=106,"t_trig":<-100]
  ,Synth 1106 "cd2tnzfb2"
   ["outa":=107,"outf":=108,"t_trig":<-100]
  ,Synth 1107 "cd2tdrn2"
   ["outg":=109,"outf":=110,"t_trig":<-100]]

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
  -- ,Synth 2005 "cd2nzf" 
  --  ["out":=14,"t_amp":<-105,"freq":<-106]
  -- ,Synth 2006 "cd2nzf" 
  --  ["out":=15,"t_amp":<-107,"freq":<-108]
  -- ,Synth 2007 "cd2drn"
  --  ["out":=16,"amp":=0.3,"gate":<-109,"freq":<-110]
  ,Synth 2008 "playU"
   ["out":=17,"t_trig":<-100]]
   
-- effects  
n21 = 
  Group 21
  [Synth 2101 "cd2rev"
   ["out":=10,"a_in":<=10,"dlyt":=0.04801,"dmul":=0.01]
  ,Synth 2102 "cd2rev"
   ["out":=12,"a_in":<=12,"dlyt":=0.08,"dmul":=0.02]
  ,Synth 2103 "cd2rev"
   ["out":=14,"a_in":<=14,"dlyt":=0.12191,"dmul":=0.0181]
  ,Synth 2104 "cd2rev"
   ["out":=15,"a_in":<=15,"dlyt":=0.08,"dmul":=0.03]]

-- mix
n90 =
  Group 90
  [Synth 9001 "cd2mix" -- huh
   ["out":=0,"a_in":<=10,"amp":=2.4,"pan":=0.0]
  ,Synth 9002 "cd2mix" -- hat
   ["out":=0,"a_in":<=11,"amp":=0.1,"pan":=(-0.2)]
  ,Synth 9003 "cd2mix" -- snr
   ["out":=0,"a_in":<=12,"amp":=0.8,"pan":=(-0.1)]
  ,Synth 9004 "cd2mix" -- kik
   ["out":=0,"a_in":<=13,"amp":=1.2,"pan":=0.03]
  ,Synth 9005 "cd2mix" -- nzf a
   ["out":=0,"a_in":<=14,"amp":=0.13,"pan":=0.5] -- amp=0.13
  ,Synth 9006 "cd2mix" -- nzf b
   ["out":=0,"a_in":<=15,"amp":=0.2,"pan":=(-0.5)] -- amp=0.2
  ,Synth 9007 "cd2mix" -- nzdrn
   ["out":=0,"a_in":<=16,"amp":=0.0,"pan":=0]
  ,Synth 9008 "cd2mix" -- nzdrn
   ["out":=0,"a_in":<=17,"amp":=1.0,"pan":=0]]

w = withSC3

respond :: Transport t => t -> IO b
respond fd = do
  async fd (notify True)
  forever $ do
    r <- wait fd "/cd22e9d"
    work r fd

work :: Transport t => OSC -> t -> IO ()
work (Message "/cd22e9d" [Int _,Int _,Float v]) fd =
  let n = ceiling v :: Int
      (q,r) = n `quotRem` 64
  in  case r of
    0 -> case q of
      q | q `mod` 4 == 1 -> patchNode n11b fd
        | otherwise      -> patchNode n11a fd
    _ -> return ()