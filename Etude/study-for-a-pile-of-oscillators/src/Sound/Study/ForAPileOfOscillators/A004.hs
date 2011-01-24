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
-- Playing with a pile of oscillators, take 4.
--
module Sound.Study.ForAPileOfOscillators.A004 where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.List (transpose, groupBy)
import Data.Traversable (sequenceA)
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton
import Sound.SC3.Lepton.GUI

import Control.Exception (bracket)

import Sound.Study.ForAPileOfOscillators.Common

main = withSC3 $ \fd -> 
  treeToGui (Group 0 (afp4++[smasterNode])) hints' fd
  where
    hints' = M.union hints $ M.fromList $
      map (\x -> (x,h)) ["oc1_1","oc1_2","oc1_3","oc1_4"]
    h = [ParamRange "freq" 5 8000
        ,ParamRange "amp" 0 0.2
        ,ParamRange "pan" (-1) 1]
        
writeA004Score :: FilePath -> IO ()        
writeA004Score path = error "not yet written"
        
w :: (UDP -> IO a) -> IO a
w = withSC3        

setup :: (Transport t) => t -> IO OSC
setup fd = do
  mapM_ (\(n,u) -> loadSynthdef n u fd)
    [("oc1_1", oc1_1),("oc1_2",oc1_2),("oc1_3",oc1_3),("oc1_4",oc1_4)
    ,("fc2_1",fc2_1),("fc2_2",fc2_2) 
    ,("metro004",metro004),("t0042",t0042),("k004",k004)]
  async fd $ b_free pitchBuf
  async fd $ b_alloc pitchBuf (length pitches) 1
  send fd $ b_setn pitchBuf [(0,pitches)]
  reloadSynthdef fd
  
go fd = do
  addNode 0 a004Nodes fd
  goTrig fd
    
-- | Change values of pitch buffer.
goPitchBuf :: (Transport t)
           => (Double -> Double) -- ^ Function mapped to pitches
           -> t -> IO ()
goPitchBuf f fd = do
  async fd $ b_alloc pitchBuf (length pitches) 1
  send fd $ b_setn pitchBuf [(0,map f pitches)]

goTrig fd = do
  m1s <- runPIO $ sequenceA pat001
  m2s <- runPIO $ sequenceA pat002
  let z = ZipList
  sequence_ $ getZipList $ f <$> z m1s <*> z m2s
  where
    f m1 m2  = do
      now <- utcr
      let m2' = M.assocs m2
      send fd $ Bundle (UTCr $ now + 0.1)
        [n_set 1002 (M.assocs m1),n_set 1105 m2',n_set 1106 m2']
      threadDelay (floor $ 1e6 * (1/4) * beat)

bpm :: (Num a) => a
bpm = 72

beat :: (Fractional a, Num a) => a
beat = 60/bpm

-- | C minor penta tonic.
pitches :: (Num a) => [a]
pitches = map (+36) $ zipWith (+) (cycle [0,3,5,7,10])
  (concatMap (replicate 5) [0,12,24,36,48,60,72,84])
          
--
-- Patterns
--

pat001 = M.fromList
   [("t_t1", pcycle [0,1,0,0, 1,0,0,0, 0,1,0,0, 0,0,0,1])
   ,("t_t2", pcycle [1,0,0,0, 0,0,1,0, 1,0,0,0, 0,1,0,0])
   ,("t_t3", pcycle [0,0,0,1, 0,1,0,0, 0,0,0,1, 0,0,1,0]) 
   ,("t_t4", pcycle [0,0,1,0, 0,0,0,1, 0,0,1,0, 1,0,0,0])]
   
pat002 = M.fromList
  [("t_trig", pcycle [pseq 60 [0], plist [0,1,0,0]])]
  
--
-- Node mappings
--

a004Nodes =
  Group 0
    [Group 1
       [Group 10 k004s
       ,Group 11 afp4
       ,Group 12 oscs
       ,Group 13 [smasterNode]]]

k004s =
  [Synth 1002 "t0042" []]
  
-- k004s =   
--   [Synth 1000 "metro004" ["bpm":=bpm,"out":=t1000b]
--   ,Synth 1001 "k004" ["t_trig":<-t1000b]]

afp4 =
  [Synth 1101 "oc1_1"
     ["amp":=0.03,"freq":=239.3,"pan":=0.9,"atk":=2e-3,"rel":=308e-3
     ,"t_trig":<-t1101b]
  ,Synth 1102 "oc1_2"
     ["amp":=0.03,"freq":=1307.98,"pan":=0.3,"atk":=8e-3,"rel":=271e-3
     ,"t_trig":<-t1102b]
  ,Synth 1103 "oc1_3"
     ["amp":=0.03,"freq":=761.8,"pan":=(-0.3),"atk":=3e-3,"rel":=250e-3
     ,"t_trig":<-t1103b]
  ,Synth 1104 "oc1_4"
     ["amp":=0.03,"freq":=81.3,"pan":=(-0.9),"atk":=12e-3,"rel":=333e-3
     ,"t_trig":<-t1104b]
  ,Synth 1105 "fc2_1"
     ["bufnum":=pitchBuf,"lagt":=5e-3,"t_trig":<-t1000b]
  ,Synth 1106 "fc2_2"
     ["bufnum":=pitchBuf,"lagt":=8*beat,"t_trig":<-t1000b]]

smasterNode =
  Synth 1301 "smaster"
     ["amp":=1,"rmix":=0.01,"rroom":=0.85,"rdamp":=0.75]

--
-- Oscillator ids
-- 

bank1 = filter (\x -> (x `mod` 8) `elem` [0..3]) oscIds
(bank1a:bank1b:bank1c:bank1d:_) = transpose $ groupBy (\x y -> (y-x) < 5) bank1
bank2 = filter (\x -> (x `mod` 8) `elem` [4..7]) oscIds

--
-- Bus ids
--

t1000b = 100
t1101b = 101
t1102b = 102
t1103b = 103
t1104b = 104

-- 
-- Buffer ids
--
pitchBuf = 1

--
-- UGens
--

metro004 = out outBus tr
  where
    outBus = ctrl "out" 0
    tr = impulse kr freq 0
    freq = bpm/60
    bpm = ctrl "bpm" 72

k004 = mrg outs
  where
    outs = [out t1101b t1, out t1102b t2, out t1103b t3, out t1104b t4]
    t1 = pd t0 16 0 + pd t0 16 12
    t2 = pd t0 16 2 + pd t0 16 8
    t3 = pd t0 16 4 + pd t0 16 14
    t4 = pd t0 16 6 + pd t0 16 10
    t0 = foldr f t_trig [1..8]
    f a b = delayN t_trig (60/bpm) ((a/8)*(60/bpm)) + b
    pd = pulseDivider        
    t_trig = ctrl "t_trig" 0
    
t0042 = mrg outs
  where
    outs = [out t1101b t1, out t1102b t2, out t1103b t3, out t1104b t4]
    t1 = ctrl "t_t1" 0
    t2 = ctrl "t_t2" 0
    t3 = ctrl "t_t3" 0
    t4 = ctrl "t_t4" 0

(oc1_1:oc1_2:oc1_3:oc1_4:_) = map oc1 [bank1a,bank1b,bank1c,bank1d]

fc2_1,fc2_2 :: UGen
fc2_1 = fc2 bank1
fc2_2 = fc2 bank2

--
-- UGen makers
--

-- | Controls amp, freq, and pan of oscillator specified with node id.
oc1 :: [NodeId] -> UGen
oc1 = mrg . concatMap mkO
  where
    mkO i = [out (ampBus i) (amp i)
            -- ,out (freqBus i) (freq i)
            ,out (panBus i) pan]
    amp i = ampEnv * ampC i
    ampEnv = envGen kr t_trig 1 0 1 DoNothing $
             env [0,0,1,0] [0,atk,rel] [EnvCub] (-1) 0
    atk = ctrl "atk" 5e-3
    rel = ctrl "rel" 300e-3
    ampC i = ctrl "amp" 0.05 * tRand i 0.95 1.05 t_trig
    -- freq i = ctrl "freq" 440 * (0.9 + tExpRand i 1e-3 100e-3 t_trig)
    pan = ctrl "pan" 0 
    t_trig = ctrl "t_trig" 0
    
-- | Look up a buffer for pitch value and use for each oscillators.
fc2 :: [NodeId] -> UGen
fc2 = mrg . map mkO
  where
    mkO i = out (freqBus i) (freq i)
    freq i = lag (targetFreq i) (lagt * tExpRand i 0.25 4 t_trig)
    targetFreq i = midiCPS (index bufn (idx i)) * detune i
    detune i = tRand i 0.9998 1.0002 t_trig
    -- idx i = tiRand i 0 (fromIntegral $ length pitches - 1)
    --         (tDelay (pulseDivider t_trig 16 (-2)) 
    --          (tExpRand (succ i) 0.25 4 t_trig))
    idx i = tiRand i 0 (fromIntegral $ length pitches - 1)
            (tDelay t_trig (tExpRand (succ i) 0.25 4 t_trig))
    bufn = ctrl "bufnum" 1
    lagt = ctrl "lagt" (2*60/bpm)
    t_trig = ctrl "t_trig" 0