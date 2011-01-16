------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Temporary module to hold experimental stuffs.
--
module Sound.Etude.StudyForAPileOfOscillators.Tmp where

import Control.Monad (forever)
import Data.List (zipWith4)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.Etude.StudyForAPileOfOscillators.A001

------------------------------------------------------------------------------
--
--
-- UGens
--
--
------------------------------------------------------------------------------

-- | UGen to send sendTrig message periodically.
metro :: UGen
metro = mrg [sendTrig tr 1 val]
  where
    tr = impulse kr bpm 0
    val = pulseCount tr rst `mod` 1024
    bpm = ctrl "bpm" 60 / 60
    rst = ctrl "reset" 0

-- | UGen to send linear value to control bus.
lin :: UGen
lin = out outBus val
  where
    outBus = ctrl "out" 0
    val = line kr str end dur RemoveSynth
    str = ctrl "str" 0
    end = ctrl "end" 1
    dur = ctrl "dur" 1
    
-- | Simple ugen with arrayed control arg.
bosc :: UGen
bosc = out 0 $ pan2 sig pan 1
  where
    sig = mix (sinOsc ar freq 0) * amp
    freq = ctrls "freq" [100,330,440,880]
    amp = ctrl "amp" 0.1
    pan = ctrl "pan" 0
    
------------------------------------------------------------------------------
--
--
-- Client side control
--
--
------------------------------------------------------------------------------

-- | Set random params
rSet :: (Transport t)
      => String
      -> Double
      -> Double
      -> t
      -> IO ()
rSet name lo hi fd = do
  vals <- newStdGen >>= return . randomRs (lo,hi)
  mkSet name allIds vals fd

-- | Set value with repeating given list.
eSet :: (Transport t)
     => String
     -> [Double]
     -> t
     -> IO ()
eSet name vs fd = mkSet name allIds (cycle vs) fd

-- | Make n_set messages.
mkSet :: (Transport t)
      => String
      -> [Int]
      -> [Double]
      -> t
      -> IO ()
mkSet name ids vs fd = do
  now <- utcr
  send fd $ Bundle (UTCr $ now + 1) $
    zipWith (\n v -> n_set n [(name,v)]) ids vs

lins :: (Transport t)
     => [Int] -> [Double] -> [Double] -> [Double] -> t -> IO ()
lins is ss es ds fd = send fd $ Bundle (NTPi 0) ms
  where
    ms = zipWith4 (\o s e d -> s_new "lin" (-1) AddToTail 10
                               [("out",fromIntegral o),("str",s)
                               ,("end",e),("dur",d)])
         is ss es ds

-- | Send a message from client to server as response of sendTrig ugen.
--
-- Try:
--
-- > > withSC3 $ asResponse responder
--
asResponse :: (Transport t) => (Double -> t -> IO ()) -> t -> IO ()
asResponse act fd = async fd (notify True) >> forever g
  where
    g = do
      Message "/tr" [Int _,Int 1,Float b] <- wait fd "/tr"
      act b fd

responder :: (Transport t) => Double -> t -> IO ()
responder n fd
  | at 0 = do
    print n
    f 1001 [("tick",1),("noise",0),("sin",0),("t_trig",1) ,("vc",30e-3)
           ,("vd",25e-3)]
    f 1002 [("sin",1),("noise",0),("t_trig",1),("vc",1200),("vd",800)]
  | at 8 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
    f 1003 [("t_trig",1)]
  | at 16 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 17 = do
    f 1002 [("sin",0),("noise",1)]
  | at 32 = do
    f 1002 [("vc",800),("vd",20)]
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 48 = do
    f 1001 [("vc",30e-3),("vd",25e-3)]
  | at 54 = do
    f 1001 [("tick",0),("noise",1)]
  | at 60 = do
    f 1002 [("vc",4000),("vf",4000)]
  | otherwise = return ()
  where
    at k = floor n `mod` 64 == k
    f n p = send fd $ n_set n p

ws :: (Transport t) => Double -> t -> IO ()
ws d fd = w1 d fd >> w2 d fd >> w3 d fd

-- | Worker for amp.
w1 :: (Transport t) => Double -> t -> IO ()
w1 n fd
  | at 0 = hit (const True) >> print n
  | at 1 = hit odd
  | at 2 = hit even
  | at 3 = hit even
  | at 4 = do
    as <- randomRIOs (1e-3,3e-2)
    ae <- randomRIOs (1e-3,3e-2)
    lins aBusses as ae (repeat 1) fd
  | at 5 = hit even
  | at 6 = hit odd
  | at 7 = hit (const True)
  | at 8 = hit even
  | at 9 = hit odd
  | at 10 = do
    let as = repeat 0
    ae <- randomRIOs (1e-3,3e-2)
    ds <- randomRIOs (5e-3,1e-2)
    lins aBusses as ae ds fd
  | at 11 = hit odd
  | at 12 = do
    let ds = repeat 5e-3
    let as = repeat 0
    ae <- randomRIOs (1e-3,3e-2)
    lins (filter odd aBusses) as ae ds fd
  | at 13 = hit even
  | at 14 = hit even
  | at 15 = hit even
  | otherwise = return ()
  where
    at k = floor n `mod` 16 == k
    hit p = do
      ds <- randomRIOs (5e-3,5000e-3)
      as <- randomRIOs (1e-3,3e-2)
      lins (filter p aBusses) as (repeat 0) ds fd

-- | Worker for frequency.
w2 :: (Transport t) => Double -> t -> IO ()
w2 n fd
  | at 0 = do
    f <- randomRIO (80,120)
    ds <- randomRIOs (50e-3,1e2)
    let fs = zipWith (*) [1..] (repeat f)
    lins fBusses fs (zipWith (*) fs (cycle [0.99,1.01])) ds fd
  | at 7 = do
    let fs = repeat 0
    fe <- randomRIOs (100,800)
    lins (filter odd fBusses) fs fe (repeat 0.3) fd
  | at 15 = do
    f <- randomRIO (80,120)
    let fs = zipWith (*) [1..] (repeat f)
    lins fBusses fs (zipWith (*) fs (cycle [0.99,1.01])) (repeat 2) fd
  | at 19 = do
    fs <- randomRIOs (100,8000)
    ds <- randomRIOs (500e-3,8000e-3)
    let fe = repeat 0
    lins fBusses fs fe ds fd
  | otherwise = return ()
  where
    at k = floor n `mod` 23 == k

-- | Worker for pan.
w3 :: (Transport t) => Double -> t -> IO ()
w3 n fd
  | at 0 = do
    pe <- randomRIOs (-1,1)
    lins pBusses (repeat 0) pe (repeat 8) fd
  | otherwise = return ()
  where
    at k = floor n `mod` 7 == k
