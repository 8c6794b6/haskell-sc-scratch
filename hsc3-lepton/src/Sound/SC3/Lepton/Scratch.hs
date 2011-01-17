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
-- Scratch!!!
--

module Sound.SC3.Lepton.Scratch where

import Control.Concurrent (forkIO, threadDelay)
import System.Random (newStdGen, randomRs)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID

import Sound.SC3.Lepton.Pattern
import Sound.SC3.Lepton.Tree

------------------------------------------------------------------------------
--
-- Patterns
--
------------------------------------------------------------------------------

main = withSC3 go

-- | Load synth def and play the pattern.
go :: (Transport t) => t -> IO ()
go fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  mapM_ f =<< runPIO pspe'
  where
    f v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ 0.13 * 1e6)

-- | Synthdef for spe example.
speSynth :: IO UGen
speSynth = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ mkSig dl dr
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw ar freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen kr 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' kr 1 * 36 + 110)
    freq = control kr "freq" 440

pspe' =
  pcycle
  [prand 1 [pempty, plist [24,31,36,43,48,55]]
  ,pseq (prand 1 ([2..5]))
   [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
  ,prand (prand 1 [3..9]) [74,75,77,79,81]]

pspe =
  pcycle
    [prand (pval 1)
     [pempty, plist [24,31,36,43,48,55]]

    ,pseq (prand (pval 1) [pval 2, pval 3, pval 4, pval 5])
     [pval 60
     ,prand (pval 1) [pval 63, pval 65]
     ,pval 67
     ,prand (pval 1) [pval 70, pval 72, pval 74]]

    ,prand (prand (pval 1)
            [pval 3, pval 4, pval 5, pval 6, pval 7, pval 8, pval 9])
     [pval 74, pval 75, pval 77, pval 79, pval 81]]


------------------------------------------------------------------------------
--
-- Synth Nodes
--
------------------------------------------------------------------------------

-- | Sample message returned from scsynth server, without group other
-- than default.
oscList1 :: OSC
oscList1 = Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 1,Int 2, -- default group, with 2 child elements
                 Int 1000,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0,
                 Int 1001,Int (-1),
                 String "simplePercSine",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",Float 0.0,
                 String "amp",Float 0.10000000149011612,
                 String "freq",Float 440.0,
                 String "out",Float 0.0]

tree1 :: SCNode
tree1 =
  Group 0
    [Group 1
      [Synth 1000 "simplePercSine"
        ["sustain" := 0.800000011920929,
         "trig" :<- 101,
         "amp" := 0.10000000149011612,
         "freq" := 440,
         "out" := 0],
       Group 10
         [Group 100
           [Group 101
             [Synth 1011 "simplePercSine"
              ["sustain" := 0.8,
               "trig" :<- 102,
               "amp" := 0.1,
               "freq" := 330,
               "out" := 0]]]],
       Synth 1001 "simplePercSine"
         ["sustain" := 0.800000011920929,
          "trig" :<- 103,
          "amp" := 0.10000000149011612,
          "freq" := 440,
          "out" := 0]]]

oscList2 :: OSC
oscList2 =
    Message "/g_queryTree.reply"
                [Int 1, -- containing control parameters
                 Int 0,Int 1, -- root node, with 1 child element
                 Int 1,Int 5, -- default group, with 5 child element
                 Int 2,Int 0, -- group 2, no child element
                 Int 3,Int 4, -- group 3, 4 child elements.
                 Int 1002,Int (-1), -- node 1002,
                 String "param",Int 4, -- name is "param", 4 control params.
                 String "idx",String "c103", -- idx, from control bus 103.
                 String "parambuf",Float 10.0,
                 String "trig",String "c102",
                 String "out",Float 100.0,
                 Int 1003,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c103",
                 String "parambuf",Float 12.0,
                 String "trig",String "c102",
                 String "out",Float 101.0,
                 Int 1004,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 20.0,
                 String "trig",String "c202",
                 String "out",Float 200.0,
                 Int 1005,Int (-1),
                 String "param",Int 4,
                 String "idx",String "c203",
                 String "parambuf",Float 22.0,
                 String "trig",String "c202",
                 String "out",Float 201.0,
                 Int 4,Int 2,
                 Int 1000,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 0.800000011920929,
                 String "trig",String "c102",
                 String "amp",String "c100",
                 String "freq",String "c101",
                 String "out",Float 104.0,
                 Int 1001,Int (-1),
                 String "para4",Int 5,
                 String "sustain",Float 2.0,
                 String "trig",String "c202",
                 String "amp",String "c200",
                 String "freq",String "c201",
                 String "out",Float 204.0,
                 Int 5,Int 2,
                 Int 1006,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.5,
                 String "room",Float 0.8999999761581421,
                 String "mix",Float 0.5,
                 String "in",Float 104.0,
                 Int 1007,Int (-1),
                 String "simpleReverb",Int 4,
                 String "damp",Float 0.8999999761581421,
                 String "room",Float 0.5,
                 String "mix",Float 0.5,
                 String "in",Float 204.0,
                 Int 6,Int 2,
                 Int 1008,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float (-0.800000011920929),
                 String "gain",Float 1.0,
                 String "bus",Float 104.0,
                 Int 1009,Int (-1),
                 String "simplePanGain",Int 3,
                 String "pan",Float 0.800000011920929,
                 String "gain",Float 1.0,
                 String "bus",Float 204.0]

oscList3 :: OSC
oscList3 =
    Message "/g_queryTree.reply"
    [Int 1,
     Int 1, Int 2,
     Int 2, Int 0,
     Int 3, Int 0]
