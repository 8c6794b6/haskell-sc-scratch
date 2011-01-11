------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_thunder.html>
--
-- Try:
--
-- > > withSC3 reset
-- > > runThunder
-- > > audition $ wind
--
module Pssd.Earth.Thunder where

import Control.Concurrent (forkIO, threadDelay)
import Data.List (zipWith4)
import System.Random (newStdGen, randomRs)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Run thunder.
runThunder :: IO ()
runThunder = do
  withSC3 $ \fd -> do
    async fd $ d_recv $ synthdef "bolt" bolt
    async fd $ d_recv $ synthdef "afterImages" afterImages
    async fd $ d_recv $ synthdef "boom" boom
  forkIO $ withSC3 mkBolts
  threadDelay (floor $ 0.1 * 1e6)
  withSC3 $ \fd -> send fd $ s_new "afterImages" (-1) AddToTail 1 [("t_trig",1)]
  threadDelay (floor $ 0.9 * 1e6)
  withSC3 $ \fd -> send fd $ s_new "boom" (-1) AddToTail 1 [("t_trig",1)]

-- | Bolt sound.
bolt :: UGen
bolt = out 0 $ mce2 sig sig
  where
    sig = bpf sig1 2500 0.5 * 0.2
    sig1 = sum [ bpf nz (3600 * pass) 7e-1 * 2
               , bpf nz (4600 * pass) 7e-1 * 2
               , bpf nz (7200 * pass) 8e-1 * 2
               , bpf nz (480 * pass) 7e-1 * 1.2
               , bpf nz (720 * pass) 5e-1 * 2.5
               , bpf nz (80 * pass) 3e-1 * 10 ]
    nz = clip (whiteNoise 'a' ar * curve * 10) (-0.3) 3
    curve = xLine kr 1 1e-9 dur RemoveSynth
    pass = ctrl "pass" 1
    dur = ctrl "dur" 8e-3

-- | Load synthdefs defined in this module.
loadThunder :: (Transport t) => t -> IO [OSC]
loadThunder = \fd -> do
  let f (n,u) = async fd . d_recv $ synthdef n u
  mapM f [("bolt",bolt)
         ,("bolt2",bolt2)
         ,("afterImages",afterImages)
         ,("boom",boom)
         ,("bTrig",bTrig)]

-- | Synth node graph for bolt, afterImages, and boom.
--
-- All synthdefs are triggered with output from dust ugen.
thunderGraph :: SCTree
thunderGraph =
  Group 0
    [Group 1
      [Group 10
        [Synth 1001 "bTrig" ["out" := fromIntegral tBus
                            ,"freq" := 0.5]]
      ,Group 20
        [Synth 2001 "bolt2" ["t_trig" :<- tBus]
        ,Synth 2002 "afterImages" ["t_trig" :<- tBus]
        ,Synth 2003 "boom" ["t_trig" :<- tBus]]]]
  where
    tBus = 100

-- | Trigger for thunder sound.
bTrig :: UGen
bTrig = out outBus t
  where
    outBus = ctrl "out" 100
    t = dust 'a' kr (ctrl "freq" 0.5)

-- | Bolt sound made from sequencial percussive noise.
bolt2 :: UGen
bolt2 = out2 $ bpf sig 2500 0.5 * ctrl "amp" 0.2
  where
    sig = sum $ zipWith3 mkBolt2 ts ps ds
    ts = foldr (\x d -> tDelay tr x:d) []
         $ zipWith4 tRand ['a'..] [0.0125,0.025..] [0.025,0.05..] (repeat tr)
    ps = map (\x -> x/10) ns
    ds = map (\x -> 4/x) ns
    ns = [20,19..1]
    tr = ctrl "t_trig" 1

-- | Single percussive noise for bolt sound.
mkBolt2 :: UGen -- ^ Trigger
        -> UGen -- ^ Pass
        -> UGen -- ^ Duration
        -> UGen
mkBolt2 trg pass dur = sig * 0.2
  where
    sig = sum [ bpf nz (3600 * pass) 7e-1 * 2
              , bpf nz (4600 * pass) 7e-1 * 2
              , bpf nz (7200 * pass) 8e-1 * 2
              , bpf nz (480 * pass) 7e-1 * 1.2
              , bpf nz (720 * pass) 5e-1 * 2.5
              , bpf nz (80 * pass) 3e-1 * 10 ]
    nz = clip (whiteNoise 'a' ar * curve * 10) (-0.3) 3
    curve = cubed $ cubed $ linen trg 1e-9 1 dur DoNothing

-- | Play initial bolt sounds.
--
-- Sequencial sending s_new messages with changing params.
--
mkBolts :: (Transport t) => t -> IO ()
mkBolts fd = do
  g <- newStdGen
  let ds = scanl (+) 0 $ randomRs (5,20::Double) g
      f n d = do
        threadDelay (floor $ d * 3e2)
        send fd $ s_new "bolt" (-1) AddToTail 1
          [("pass",n/10),("dur",4/n)]
  sequence_ $ zipWith f [10,9..1] ds

-- | After image reverb sound.
--
-- Using low pass filter rather than band pass for last applied filter.
--
afterImages :: UGen
afterImages = out2 sig
  where
    sig = lpf sig1 330 * amp * ctrl "amp" 1
    sig1 = clip (n1 * n2) (-1) 1
    amp = squared $ linen td 0 1 6 DoNothing
    td = tDelay (ctrl "t_trig" 0) 0.2
    n1 = whiteNoise 'a' ar
    n2 = lpf (whiteNoise 'b' ar) freq * 80
    freq = 5 + cubed (linen td 0 1 1 DoNothing) * 120

-- | Low frequency boom sound.
boom :: UGen
boom = out2 sig
  where
    sig = n1 * amp * ctrl "amp" 1
    n1 = lpf (hpf (bBandPass (whiteNoise 'a' ar) 80 0.2) 5) 200 * 10
    amp = clip (squared ampShape) 0 0.77
    ampShape = envGen kr td 1 0 1 DoNothing $
               env [2000,2000,1e-9] [0,20] [EnvExp] (-1) (-1)
    td = tDelay (ctrl "t_trig" 0) 1

-- | Wind.
wind :: UGen
wind = out2 $ sig * ctrl "amp" 1.0
  where
    sig = lpf (bpf n1 1000 0.7) 8000
    n1 = n2 * n3
    n2 = squared nCtr + 0.5
    n3 = resonz nSrc nFreq1 0.35 + resonz nSrc nFreq2 0.2
    nSrc = whiteNoise 'a' ar
    nCtr = lfNoise2 'f' ar 1
    nFreq1 = clip (lfNoise2 'b' ar 0.3 * 8000) 0 1000 + o
    nFreq2 = clip (lfNoise2 'c' ar 0.49 * 16000) 0 1000 + o
    o = o' * 350 + 1000
    o' = sinOsc ar 0.03 0 + sinOsc ar 0.3 0

-- | Wind sound, take 1.
wind1 :: UGen
wind1 = out 0 $ mce2 sig sig
  where
    sig = bpf n1 1000 0.35
    n1 = resonz n2 freq 0.35
    freq = clip (lfNoise2 'f' ar 0.35 * 4000) 0 1000 + 500
    n2 = whiteNoise 'n' ar
