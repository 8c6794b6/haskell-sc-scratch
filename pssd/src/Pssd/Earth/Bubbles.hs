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
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_bubbles.html>
--
-- Try:
--
-- > > withSC3 bubbles
--
module Pssd.Earth.Bubbles where

import Control.Monad (forever)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Pssd.Util

-- | Trigger for bubble sound.
triggerBubble :: UGen
triggerBubble = mrg [out outBus sig, out trigBus triggerOut]
  where
    sig = tRand 'a' 0 100 triggerOut * 10
    triggerOut = coinGate 'b' bound triggerNewValue
    triggerNewValue = sum (map inSelection elements)
    inSelection x = (count `mod` 200) ==* x
    elements = [29,37,47,67,89,113,157,197]
    count = pulseCount (impulse kr (1/interval) 0) 0
    outBus = ctrl "out" 101
    trigBus = ctrl "trigBus" 102
    bound = ctrl "bound" 0.5
    interval = ctrl "interval" 0.015

-- | UGen for bubble sound.
aBubble :: UGen
aBubble = out 0 $ mce2 sig sig
  where
    sig = bub * amp * 20
    bub = onePole bosc (-0.98)
    bosc = sinOsc ar freq 0
    freq = envGen kr t_trig 1 0 1 doneAction fenv * k_freq
    amp = envGen kr t_trig 1 0 1 doneAction aenv
    fenv = env [1e-9,1e-9,1] [4e-2,1e-1] [EnvExp,EnvCub] (-1) 0
    aenv = env [1e-9,1,1e-9] [1e-1,3.5e-2] [EnvExp,EnvCub] (-1) 0
    t_trig = ctrl "t_trig" 1
    k_freq = ctrl "freq" 440 * 5
    doneAction = DoneAction $ ctrl "doneAction" 0

-- | Yet another bubble sound.
bBubble :: UGen
bBubble = out 0 $ mce [sig, sig]
  where
    sig = bub * amp
    bub = onePole (sinOsc ar freq 0) (-0.9)
    freq = envGen kr t_trig 1 0 1e-1 DoNothing fShape *
           ctrl "freq" 150 * 10
    amp = envGen kr t_trig 1 0 1 DoNothing aShape
    fShape = env [1e-9,1,1e-9] [0.4,1e-3] [EnvExp,EnvLin] (-1) (-1)
    aShape = env [1e-9,1e-9,1] [0,4e-1] [EnvExp,EnvLin] (-1) 0
    t_trig = ctrl "t_trig" 1

-- | Add bubble node and controller node for it, and maps them.
bubbles :: (Transport t) => t -> IO ()
bubbles fd = do
  let trigBus = 101
      outBus = 102
  async fd $ d_recv $ synthdef "triggerBubble" triggerBubble
  async fd $ d_recv $ synthdef "aBubble" aBubble
  tid <- s_new_id fd "triggerBubble" AddToTail 1
         [ ("out", fromIntegral outBus)
         , ("trigBus", fromIntegral trigBus)
         , ("interval", 0.0015)
         , ("bound", 0.9) ]
  bid <- s_new_id fd "aBubble" AddAfter tid []
  send fd $ n_map bid
    [ ("t_trig", trigBus)
    , ("freq", outBus)]

-- | Run bubble sounds.
--
-- This action receives trigger message from @triggerBubble@ synth,
-- and then it's passed to @sendTrig@ ugen, then sending s_new
-- message as response.
--
newBubbles :: (Transport t) => t -> IO ()
newBubbles fd = do
  let trigBus = 101
      outBus = 102
      bubbleTriggerId = 3238920
      respondWithBubble = do
        Message "/tr" (_:Int bubbleTriggerId:Float f:_) <- wait fd "/tr"
        send fd $ s_new "aBubble" (-1) AddToTail 1
          [ ("freq", f/2), ("doneAction", 2), ("t_trig", 1) ]
  async fd $ notify True
  async fd $ d_recv $ synthdef "triggerBubble" triggerBubble
  async fd $ d_recv $ synthdef "aBubble" aBubble
  async fd $ d_recv $ synthdef "getTriggerBubble" $
    sendTrig (in' 1 kr trigBus) 1 (in' 1 kr outBus)
  tid <- s_new_id fd "triggerBubble" AddToTail 1
         [ ("out", fromIntegral outBus)
         , ("trigBus", fromIntegral trigBus)
         , ("interval", 15e-4)
         , ("bound", 0.9) ]
  send fd $ s_new "getTriggerBubble" (-1) AddAfter tid []
  forever respondWithBubble