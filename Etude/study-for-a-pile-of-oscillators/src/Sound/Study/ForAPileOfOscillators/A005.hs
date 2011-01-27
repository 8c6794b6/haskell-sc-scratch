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
-- Playing with a pile of oscillators, take 5.
--
module Sound.Study.ForAPileOfOscillators.A005 where

import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Sound.Study.ForAPileOfOscillators.Common

main = withSC3 $ \fd -> do
  treeToGui (Group 0 (k0051:aNodes)) hints' fd
  where
    k0051 = Synth 1001 "k0051" ["freq":=1,"chaos":=0.5]
    hints' = hints `M.union` M.fromList
             [("k0051",[ParamRange "freq" 0 12])]

setup fd = do
  mapM_ (\(n,u) -> writeSynthdef n u) defs
  reloadSynthdef fd

go fd = addNode 0 a005Nodes fd

defs = [("ac4_1", ac4_1),("ac4_2",ac4_2),("k0051",k0051),("k0050",k0050)]

writeA005Score path = writeNRT path $ initial ++ last
  where
    initial = map (\m -> Bundle (NTPr 0) [m]) (treeToNew 0 a005Nodes)
    last = [Bundle (NTPr 230)
             [n_set 1101 [("amp",0),("alag",10)]
             ,n_set 1102 [("amp",0),("alag",10)]]
           ,Bundle (NTPr 241) []]

a005Nodes =
  Group 1
    [Group 10 kNodes
    ,Group 11 aNodes
    ,Group 12 oscs]

kNodes =
  [Synth 1000 "k0050"
     ["outf":=n1000f, "outi":=n1000i]
  ,Synth 1001 "k0051"
     ["out1":=n1100t,"out2":=n1101t,"freq":<-n1000f,"chaos":=0.5]]

aNodes =
  [Synth 1101 "ac4_1"
     (["amp":=1,"t_trig":<-n1100t,"fidx":<-n1000i] ++ ac4defaults)
  ,Synth 1102 "ac4_2"
     (["amp":=1,"t_trig":<-n1101t,"fidx":<-n1000i] ++ ac4defaults)]

ac4defaults = [node_k_name n := node_k_default n
              | n <- controls $ synth ac4_1
              , node_k_name n /= "amp"
              , node_k_name n /= "t_trig"]

n1100t = 100
n1101t = 101
n1300a = 102
n1000f = 103
n1000i = 104

k0050 = k0050' ("outf"=:103) ("outi"=:104)
k0050' obus ibus = mrg [out obus fsig,out ibus isig]
  where
    fsig = mkEnv EnvLin 1 1 [(0,4),(4,0.8),(120,0.5),(180,1),(210,2),(232,32)]
    isig = mkEnv EnvLin 1 1 [(0,0),(140,0),(190,0.1),(220,0)]

k0051 = k0051' ("out1"=:100) ("out2"=:101) ("freq"=:0.25) ("chaos"=:0.5)
k0051' obus1 obus2 freq chaos = out obus sig
  where
    obus = select (toggleFF sig) (mce [obus1, obus2])
    sig = coinGate 'i' (1-chaos) (impulse kr freq 0) +
          coinGate 'd' chaos (dust 't' kr freq)

ac4_1 = ac4 $ filter even oscIds
ac4_2 = ac4 $ filter odd oscIds
ac4 oids = ac4' oids ("t_trig"=:1) ("amp"=:1) ("alag"=:1) ("fidx"=:0)
  ("fc"=:0.5) ("fd"=:999e-3) ("dt"=:1) ("curve"=:0.5)
ac4' oids t_trig amp alag fidx fc fd dt curve = mrg outs
  where
    outs = concatMap mkO oids
    mkO i = [out (ampBus i) ampEnv,out (freqBus i) freq,out (panBus i) pan]
      where
        ampEnv = envGen kr dtr amp' 0 1 DoNothing $
                 env [0,0,1,0] [0,atk,rel] [EnvNum curve'] (-1) 0
        atk = tExpRand (oids !! 0) 2e-3 1 t_trig
        rel = tExpRand (oids !! 1) 2e-3 1 t_trig
        amp' = tExpRand i 1e-2 4e-2 t_trig * (lag amp alag)
        freq = freqB + (fidx * (sinOsc kr (freqB/2) 0 * (freqB/0.499)))
        freqB = tExpRand i 20 14000 t_trig
        pan = clip2 (panc + pand) 1
        panc = tRand (oids !! 2) (-1) 1 t_trig
        pand = tRand i (- (abs panc)) (abs panc) t_trig
        dtr = tDelay t_trig dtime
        dtime = tExpRand (oids !! 3) 5e-6 1 t_trig * ival * dt
        curve' = linLin curve 0 1 (-15) 15
        ival = fromIntegral (i-minimum oids)