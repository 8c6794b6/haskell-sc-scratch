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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_morephones.html>
--
-- Try:
--
-- > > withSC3 reset
-- > > withSC3 ringBell
--
module Pssd.TelephoneBells.OldStyle where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Synthdef for old style bell ringing tone.
bell :: UGen
bell = out 0 $ pan2 (sig*2) 0 1
  where
    sig = hpf (clip (sig'+strike') (-0.9) 0.9) 10
    sig' = 0.9 * sum [subs, primaries, secondaries, circular, strike]

    subs = (sinOsc ar fund 0 +
            sinOsc ar (fund*0.5001) 0 * 0.1) *
           ampEnv (600*1.2/1e3) * 0.02

    primaries = (sinOsc ar (fund*2.002) 0 * 0.2 +
                 sinOsc ar (fund*2*4.8) 0 * 0.5 +
                 sinOsc ar (fund*2*1.5) 0 * 0.1) *
                ampEnv (600*0.9/1e3) * 0.04

    secondaries = (sinOsc ar (fund*1.5*1.6666) 0 * 0.1 +
                   sinOsc ar (fund*11) 0 * 0.2 +
                   sinOsc ar (fund*1.5*1.714) 0 * 0.1) *
                  (cubed $ ((600/2)/1e3)) * 0.2

    circular = (sinOsc ar (fund*3) 0 * 0.1 +
                sinOsc ar (fund*6.242) 0 +
                sinOsc ar (fund*12.483) 0) *
               (squared $ ampEnv ((600/7)/1e3)) * 0.05

    strike = (sinOsc ar (fund*16) 0 +
              sinOsc ar (fund*24) 0 +
              whiteNoise 'a' ar) *
             (cubed $ ampEnv ((600/9)/1e3)) * 0.03

    strike' = clip bands (-0.2) 0.5
    bands = sum $ map (\x -> bpf delayB x (1/12)) [1243,287,431]
    delayB = combC delayA 0.00088 0.00088 0.1
    delayA = combC strike 0.00077 0.00077 0.1

    ampEnv d = envGen kr trg 1 0 1 DoNothing (envLinen 0 0 d 1)
    trg = ctrl "t_trig" 0
    fund = ctrl "freq" 860

-- | Trigger to ring bell.
hitBell :: UGen
hitBell = out outBus hitting
  where
    hitting = impulse kr (1/0.0333) 1 * gt
    outBus = stepper hitting 0 busNum1 busNum2 (abs $ busNum2 - busNum1) 0
    busNum1 = ctrl "outBus1" 101
    busNum2 = ctrl "outBus2" 102
    gt = ctrl "gate" 0

-- | Ring bell.
ringBell :: (Transport t) => t -> IO ()
ringBell fd = do
  async fd $ d_recv $ synthdef "bell" bell
  async fd $ d_recv $ synthdef "hitBell" hitBell
  hit <- s_new_id fd "hitBell" AddToTail 1 [("outBus1", 101), ("outBus2", 102)]
  bell1 <- s_new_id fd "bell" AddAfter hit [("gate", 1), ("freq", 860)]
  bell2 <- s_new_id fd "bell" AddAfter hit [("gate", 1), ("freq", 865)]
  send fd $ n_map bell1 [("t_trig", 101)]
  send fd $ n_map bell2 [("t_trig", 102)]
  forever (ring hit)
  where
    ring n = do
      let dur = 2
          prePause = 0.3
          pse = 2
          setGates g = n_set n [("gate", g)]
          pauseFor :: Double -> IO ()
          pauseFor t = threadDelay (floor $ t * 1000 * 1000)
      send fd (setGates 1) >> pauseFor (dur/2)
      send fd (setGates 0) >> pauseFor prePause
      send fd (setGates 1) >> pauseFor (dur/2)
      send fd (setGates 0) >> pauseFor pse
