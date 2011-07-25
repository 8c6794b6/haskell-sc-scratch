{-# LANGUAGE NoMonomorphismRestriction #-}
module Cd22e9d.Nodes2 where

import Control.Monad

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

n1 = 
  Group 0
  [Group 1
   [Group 10 -- controls
    [Synth 1000 "met9d"
     ["bpm":=260,"out":=100]
    ,Synth 1001 "tr9d"
     ["t_trig":<-100]]
   ,Group 11 -- huh
    [Synth 1101 "cd2thuh"
     ["out":=101,"t_trig":<-100]
    ,Synth 1102 "cd2huh"
     ["out":=11,"t_trig":<-101]
    ,Synth 1120 "cd2rev"
     ["out":=11,"a_in":<=11,"dlyt":=0.13,"dmul":=0.01]]
   ,Group 12 -- hat
    [Synth 1201 "cd2that"
     ["out":=120,"t_trig":<-100]
    ,Synth 1202 "cd2hat"
     ["out":=12,"t_trig":<-120]]
   ,Group 13 -- snr
    [Synth 1301 "cd2tsnr"
     ["out":=130,"t_trig":<-100]
    ,Synth 1302 "cd2snr"
     ["out":=13,"t_trig":<-130]
    ,Synth 1303 "cd2rev"
     ["out":=13,"a_in":<=13,"dlyt":=0.08,"dmul":=0.02]]
   ,Group 14 -- kik
    [Synth 1401 "cd2tkik"
     ["out":=140,"t_trig":<-100]
    ,Synth 1402 "cd2kik"
     ["out":=14,"t_trig":<-140]]
   ,n90 -- mix
   ,Group 99 -- master
    [Synth 9901 "cd2mst"
     ["out":=0,"amp":=1]]]]
  
-- Node for mix
n90 = 
  Group 90
  [Synth 9001 "cd2mix" -- huh
   ["out":=0,"a_in":<=11,"amp":=1.4,"pan":=0.0]
  ,Synth 9002 "cd2mix" -- hat
   ["out":=0,"a_in":<=12,"amp":=0.3,"pan":=(-0.1)]
  ,Synth 9003 "cd2mix" -- snr
   ["out":=0,"a_in":<=13,"amp":=0.6,"pan":=0.1]
  ,Synth 9004 "cd2mix" -- kik
   ["out":=0,"a_in":<=14,"amp":=1.2,"pan":=(-0.2)]]
  
respond :: Transport t => t -> IO b
respond fd = do
  async fd (notify True)
  forever $ do
    r <- wait fd "/s-reply"
    work r fd
    
work :: Transport t => OSC -> t -> IO ()    
work (Message "/s-reply" [Int _,Int _,Float v]) fd =
  let n = ceiling v :: Int
  in  case quotRem n 64 of
    (q,r) | r == 0 && even q -> putStrLn "pat1" >> mapM_ (flip patchNode fd) n2As
          | r == 0 && odd q  -> putStrLn "pat2" >> mapM_ (flip patchNode fd) n2Bs
          | otherwise        -> return ()
                                
n2As = [
  Group 11 -- huh
  [Synth 1101 "cd2thuh"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2huh"
   ["out":=11,"t_trig":<-101]
  ,Synth 1120 "cd2rev"
   ["out":=11,"a_in":<=11,"dlyt":=0.13,"dmul":=0.01]]
  ,Group 12 -- hat
   [Synth 1201 "cd2that"
    ["out":=120,"t_trig":<-100]
   ,Synth 1202 "cd2hat"
    ["out":=12,"t_trig":<-120]]
  ,Group 13 -- snr
   [Synth 1301 "cd2tsnr"
    ["out":=130,"t_trig":<-100]
   ,Synth 1302 "cd2snr"
    ["out":=13,"t_trig":<-130]
   ,Synth 1303 "cd2rev"
    ["out":=13,"a_in":<=13,"dlyt":=0.08,"dmul":=0.02]]
  ,Group 14 -- kik
   [Synth 1401 "cd2tkik"
    ["out":=140,"t_trig":<-100]
   ,Synth 1402 "cd2kik"
    ["out":=14,"t_trig":<-140]]
   ]

n2Bs = [
  Group 11 -- huh
  [Synth 1101 "cd2thuh2"
   ["out":=101,"t_trig":<-100]
  ,Synth 1102 "cd2huh"
   ["out":=11,"t_trig":<-101]
  ,Synth 1120 "cd2rev"
   ["out":=11,"a_in":<=11,"dlyt":=0.13,"dmul":=0.01]]
  ,Group 12 -- hat
   [Synth 1201 "cd2that2"
    ["out":=120,"t_trig":<-100]
   ,Synth 1202 "cd2hat"
    ["out":=12,"t_trig":<-120]]
  ,Group 13 -- snr
   [Synth 1301 "cd2tsnr2"
    ["out":=130,"t_trig":<-100]
   ,Synth 1302 "cd2snr"
    ["out":=13,"t_trig":<-130]
   ,Synth 1303 "cd2rev"
    ["out":=13,"a_in":<=13,"dlyt":=0.08,"dmul":=0.02]]
  ,Group 14 -- kik
   [Synth 1401 "cd2tkik2"
    ["out":=140,"t_trig":<-100]
   ,Synth 1402 "cd2kik"
    ["out":=14,"t_trig":<-140]]
   ]

w = withSC3