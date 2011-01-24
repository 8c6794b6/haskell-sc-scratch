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
-- Playing with a pile of oscillators, take 3.
--
module Sound.Study.ForAPileOfOscillators.A003 where

import Control.Concurrent (threadDelay)
import Control.Monad (zipWithM_)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Sound.Study.ForAPileOfOscillators.Common hiding (ampBus)

-- | For showing gui
main :: IO ()
main = withSC3 $ \fd -> do
  treeToGui (Group 0 (afp3++[smasterNode])) hints fd

setup :: (Transport t) => t -> IO ()
setup fd = do
  async fd $ d_recv $ synthdef "k003" k003
  addNode 0 a003Nodes fd

go :: (Transport t) => t -> IO ()
go fd = do
  setup fd
  ps <- runPIO q0
  as <- runPIO a0
  sequence_ $ zipWith3 f (repeat 1) as ps
  where
    f t a p = do
      send fd $ Bundle immediately (aBundle a p)
      threadDelay $ floor $ (60/bpm) * 1e6

writeA003Score :: FilePath -> IO ()
writeA003Score path = do
  writeSynthdef "k003" k003
  ps <- runPIO q0
  as <- runPIO a0
  writeNRT path $
    initial ++ zipWith3 f ts as ps ++ last
  where
    f t a p = Bundle (NTPr (t+0.1)) (aBundle a p)
    ts = scanl (+) 0 (repeat (60/bpm))
    initial = map (\m -> Bundle (NTPr 0) [m]) (treeToNew 0 a003Nodes)
    last = [Bundle (NTPr 422) []]
    bundleTime m = case m of Bundle (NTPr t) _ -> t; _ -> 0

bpm :: (Num a) => a
bpm = 48

-- | Make a bundled OSC message.
aBundle :: Double -> Double -> [OSC]
aBundle a p =
  [n_set 1101 []
  ,n_set 1102 [("mix", p)]
  ,n_set 1103 [("t_trig", a)]]

a003Nodes :: SCNode
a003Nodes =
  Group 0
    [Group 1
       [Group 10 [k003Node]
       ,Group 11 afp3
       ,Group 12 oscs
       ,Group 13 [smasterNode]]]

smasterNode :: SCNode
smasterNode =
  Synth 1301 "smaster"
     ["amp":<-ampBus,"rmix":=0.10,"rroom":=0.85,"rdamp":=0.75]

k003Node :: SCNode
k003Node =
  Synth 1001 "k003" []

afp3 :: [SCNode]
afp3 =
  [Synth 1101 "ac1"
     ["del":<-delBus, "vd":=0.96, "vc":=0.018, "nfreq":<-nfreqBus,
      "mix":<-amixBus ,"edgey":<-edgeyBus, "crv":=(-13), "edur":<-edurBus
     ,"tos":<-tosBus, "tmul":<-tmulBus, "tfreq":<-tfreqBus,"chaos":<-chaosBus]
  ,Synth 1102 "fc1"
     ["vib":=0, "ptc":=0, "noise":<-fnoiseBus, "vd":=995e-3, "vc":=8000
     ,"fd":=0.96, "fc":=2.94, "t_trig":=1]
  ,Synth 1103 "pc1"
     ["vd":=0.7, "vc":=0.0, "offset":=0, "sin":=0, "noise":=0.5
     ,"fc":=0.33, "fd":=0.05, "t_trig":=1]]

fnoiseBus = 100
amixBus = 101
tmulBus = 102
tosBus = 103
tfreqBus = 104
edurBus = 105
chaosBus = 106
delBus = 107
nfreqBus = 108
edgeyBus = 109
ampBus = 110

-- | Ugen for controlling a003 piece.
k003 :: UGen
k003 = mrg outs
  where
    outs = [out fnoiseBus $ e EnvSin fnoiseE
           ,out nfreqBus $ e EnvLin nfreqE
           ,out amixBus $ e EnvLin amixE
           ,out chaosBus $ e EnvLin chaosE
           ,out delBus $ e EnvLin delE
           ,out tmulBus $ e EnvLin tmulE
           ,out tfreqBus $ e EnvLin tfreqE
           ,out tosBus $ e EnvLin tosE
           ,out edurBus $ e EnvLin edurE
           ,out edgeyBus $ e EnvLin edgeyE
           ,out ampBus $ e EnvSin ampE]
    t_trig = ctrl "t_trig" 1
    e c bps = mkEnv c 1 (60/(bpm*4)) bps

writeBreakPoints :: FilePath -> IO ()
writeBreakPoints file = writeFile file $ unlines $ concatMap f
  [("fnoise", fnoiseE)
  ,("amix", amixE)
  ,("chaos", chaosE)
  ,("del", delE)
  ,("edur", edurE)
  ,("tmul", tmulE)
  ,("tfreq", tfreqE)
  ,("tos", tosE)
  ,("edur", edurE)
  ,("edgey", edgeyE)
  ,("nfreq", nfreqE)
  ,("amp", ampE)]
  where
    f (n,bps) = map (g n) bps
    g n (t,v) = unwords [show $ constantValue t, '=':n, show $ constantValue v]

fnoiseE = [(0,0),(800,0),(900,1),(980,1),(1080,0),(1110,0),(1200,1)]

amixE = [(1,1),(258,1),(750,0),(1090,1),(1122,1),(1250,0)]

chaosE = [(0,0.1),(600,0.1),(708,1),(850,1),(900,0.1)]

delE = [(0,0),(620,0),(728,0.2),(850,0.2),(900,0)]

edurE = [(0,0.723),(600,0.723),(690,1),(850,1),(900,0.8)]

tmulE = [(0,5.66),(640,5.66),(680,0.8),(850,0.8),(900,5.8)]

tfreqE= [(0,2.5),(850,2.5),(900,6)]

edgeyE = [(0,1),(900,1),(1100,0),(1280,1)]

nfreqE = [(0,0.01),(256,1),(320,0.75)]

tosE = [(0,5.11),(600,5.11),(708,0.8),(800,0.8)]

ampE = [(0,1),(1300,1),(1350,0)]

q0 =
  pseq 1
    [pseq 6
      [pseq 3 q1 ,pseq 1 q2]
    ,pseq 2 q1
    ,pseq 1 q2 ]

q1 = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7]
q2 = [0.8, 0.8, 0.8, 0.8, 0.9, 0.9, 0.9, 0.9]

a0 = pcycle [plist [1, 0, 0, 0, 0, 0, 0, 0]]