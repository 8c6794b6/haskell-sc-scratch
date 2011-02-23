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
-- Playing with a pile of oscillators, take 7.
--
module Sound.Study.ForAPileOfOscillators.A007 where

import Control.Concurrent
  (threadDelay, forkIO, killThread, newEmptyMVar, MVar, readMVar)
import Control.Monad (forM_)
import Data.Traversable (sequenceA)
import Data.List ((\\))
import Data.Map (Map, (!))
import System.Random
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Study.ForAPileOfOscillators.Common
import Sound.Study.ForAPileOfOscillators.Looper

main :: IO ()
main = putStrLn "Not yet."

setup :: (Transport t) => t -> IO OSC
setup fd = do
  mapM_ (uncurry writeSynthdef)
    [("oc71",oc71),("oc72",oc72),("pc71",pc71),("pc72",pc72)
    ,("ac71",ac71),("ac72",ac72),("ac73",ac73)]
  reloadSynthdef fd
  addNode 0 a007Nodes fd
  async fd $ b_free pitchBuf
  async fd $ b_alloc pitchBuf 256 1

a007Nodes :: SCNode
a007Nodes =
  grp 1
    [grp 11
      [syn pc72Id "pc72" ["lagt":=100e-3]
      ,syn ac71Id "ac71" []
      ,syn ac72Id "ac72" []
      ,syn ac73Id "ac73" []
      ,syn oc71Id "oc71" ["pan":=0.9,"dmax":=800e-3]
      ,syn oc72Id "oc72" ["pan":=0.8,"dmax":=1200e-3]]
    ,grp 12 oscs]

oc71Id = 1100
oc72Id = 1101
pc71Id = 1102
pc72Id = 1103
ac71Id = 1104
ac72Id = 1105
ac73Id = 1106

bpm = 120

setFreqBus fd =
  flip send $ c_setn [(head fBusses, take numOsc [100,200..])]

nsetP nid ps = do
  ms <- act $ runPIO $ sequenceA $ M.fromList ps
  forM_ ms $ \m -> do
    act $ utcr >>= \t -> withSC3 $ \fd ->
      send fd $ Bundle (UTCr (t+0.1)) [n_set nid (M.assocs m)]
    rest (m!"del")
    pauseHere

goAtk p i fd = do
  ms <- runPIO . sequenceA $ p
  forM_ ms $ \m -> do
    now <- utcr
    send fd $ Bundle (UTCr (now+0.1)) [n_set i (M.assocs m)]
    threadDelay (floor $ (60/bpm) * 1e6 * m ! "del")

pAtk1 = M.fromList
  [("t_trig",pforever (pchoose 1 [1,1,1,1,0]))
  ,("edgey",pcycle [2e-3, pchoose (prange 5 13) [998e-3,993e-3,997e-3]])
  ,("del",pforever (pchoose 1 [1/2, 1/4, 1/4, 1/4]))
  ,("mamp",pforever (prange 15e-3 3e-2))]

pAtk2 = M.fromList
  [("t_trig",pforever 1)
  ,("edgey",pforever (pchoose 1 [999e-3,800e-3,750e-3]))
  ,("del",pforever (plist [1,1/2,1/2, 1,1/2,1/2]))
  ,("mamp",pcycle [0.03, pchoose 2 [prange 1e-2 1.5e-2]
                  ,0.04, pchoose 2 [prange 1e-2 1.5e-2]])]

bank1 = filter even oscIds
bank2 = filter odd oscIds

oc71 = oc7x bank1
oc72 = oc7x bank2
oc7x oids =
  oc7x' oids ("t_trig"=:0) ("mamp"=:0.03) ("edgey"=:1) ("pan"=:0.5) ("dmax"=:800e-3)
oc7x' oids t_trig mamp edgey pan dmax = mrg $ concatMap mkO oids
  where
    mkO i = [out (ampBus i) amp, out (panBus i) pan']
      where
        amp = envGen kr t_trig mamp 0 dur DoNothing $
              env [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-10)] (-1) 0
        dur = tExpRand i 5e-3 dmax t_trig
        pan' = tRand i (-pan) pan t_trig

ac71 = ac71' ("amp"=:0)
ac71' amp = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) amp

ac72 = ac72' ("amp"=:0.003) ("freq"=:1) ("edgey"=:0.5) ("dmax"=:280e-3)
ac72' amp freq edgey dmax = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) sig
      where
        sig = envGen kr tr amp 0 dur DoNothing $
              env [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-13)] (-1) 0
        tr = dust i kr freq
        dur = tExpRand i 1e-4 dmax tr

ac73 = ac73' ("amp"=:0.003) ("freq"=:1) ("edgey"=:0.5) ("dmax"=:1000e-1)
ac73' amp freq edgey dmax = mrg $ map mkO oscIds
  where
    mkO i = out (ampBus i) sig
      where
        sig = envGen kr tr amp 0 dur DoNothing $
              env [0,0,1,0] [0,1-edgey,edgey] [EnvNum (-13)] (-1) 0
        dur = tExpRand i 1e-4 dmax tr
        tr = impulse kr freq phase
        phase = rand i 0 1

pc71 = pc71' oscIds
pc71' oids = mrg $ map mkO oids
  where
    mkO i = out (freqBus i) freq
      where
        freq = index pitchBuf (("idx_"++show i)=:i')
        i' = fromIntegral (i `mod` numOsc)

pc72 = pc72' oscIds ("t_trig"=:0) ("lagt"=:20e-3)
pc72' oids t_trig lagt = mrg $ map mkO oids
  where
    mkO i = out (freqBus i) freq
      where
        freq = envGen kr t_trig 1 0 1 DoNothing $
               env [0,0,index pitchBuf (("idx_"++show i)=:i')]
               [0,lagt] [EnvNum (-3)] (-1) 0
        i' = fromIntegral (i `mod` numOsc)

pitchBuf = 10

setPitchBuf vs = withSC3 $ \fd -> do
  async fd $ b_free pitchBuf
  async fd $ b_alloc pitchBuf 256 1
  send fd $ b_setn pitchBuf [(0,vs)]

pf1 n i = setPitchBuf $ map midiCPS $
          take numOsc $ cycle $ takeWhile (< 127) $ iterate (+n) i

rec3 :: MVar Int -> MVar Int -> Map Int Double -> Map Int Double -> Double -> Act ()
rec3 mvChg mvThr p1 p2 f = do
  now <- getNow
  act $ withSC3 $ \fd ->
    send fd $ Bundle (UTCr (now+0.1))
    [b_setn pitchBuf [(0,map midiCPS $ M.elems p1)]
    ,n_set pc72Id [("t_trig",1)]]
  rt <- act $ runPIO $ pchoose 1 [0.25,0.5,1,2,4]
  chg <- act $ readMVar mvChg
  thr <- act $ readMVar mvThr
  rest $ head rt
  pauseHere
  let diff = length $ M.elems p1 \\ M.elems p2
  if diff <= thr
    then do
      shift <- act $ runPIO $ pchoose 1 [2,5,7,10]
      let shift' = head shift
          f' = fromIntegral $ (floor $ f+shift') `mod` 12
      fs <- recSeeds f'
      -- act $ putStrLn $ "f': " ++ show f' ++ " shift': " ++ show shift'
      rec3 mvChg mvThr p2 (M.fromList $ zip [0..255] fs) f'
    else do
      idxs <- act $ runPIO $ pchoose (pval thr) [prange 0 (pval (numOsc-1))]
      let p1' = foldr (\k m -> M.update (const $ M.lookup k p2) k m) p1 idxs
      rec3 mvChg mvThr p1' p2 f

recSeeds :: Double -> Act [Double]
recSeeds shift =
  act $ runPIO $ pcycle $ map pval $
    takeWhile (< 138) $ dropWhile (< 20) $
    zipWith (+) (cycle ps) $
    concatMap (replicate $ length ps) (map (+shift) [0,12..])
  where
    -- ps = [0,3,5,7,10]
    ps = [0,4,7]

goRec3 :: MVar Int -> MVar Int -> Act ()
goRec3 mvChg mvThr = do
  f1 <- recSeeds 0
  f2 <- recSeeds 7
  let p1 = M.fromList $ zip [0..255] f1
      p2 = M.fromList $ zip [0..255] f2
  rec3 mvChg mvThr p1 p2 0

diskOut :: UGen -> UGen -> UGen
diskOut b s = mkOsc AR "DiskOut" [b, s] 1
