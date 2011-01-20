------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--

module Train where

import Data.List (isPrefixOf)

import Sound.OpenSoundControl (Transport,OSC)
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

sendTrain :: (Transport t) => t -> IO OSC
sendTrain fd = async fd $ d_recv $ synthdef "train" train
  
newTrain :: [(String,Double)] -> IO ()
newTrain ps = withSC3 $ \fd -> send fd $ s_new "train" (-1) AddToTail 1 ps

defaultTrain :: IO ()
defaultTrain = newTrain []

longAttack :: IO ()
longAttack = newTrain [("attack",1.7)]

shortAttack :: IO ()
shortAttack = newTrain [("attack",0.07),("revRoom",3.9),("revMix",0.3)]

train :: UGen
train = out 0 $ pan2 (sig*ampEnv) pan 1
  where
    sig = freeVerb (sig1 * amp) revMix revRoom 0.8
    sig1 = sig2 + sig3 + sig4
    sig2 = sinOsc ar 80 0 * 0.3
    sig3 = bpf (whiteNoise 'a' ar + pinkNoise 'a' ar * 0.5) 800 1 * saw ar 0.1
    sig4 = lpf (whiteNoise 'b' ar + pinkNoise 'b' ar * 0.25) 100 * sinOsc ar 1 0
    ampEnv = envGen ar 1 1 0 1 RemoveSynth $
             envPerc attack 1
    pan = ctrl "pan" 0
    amp = ctrl "amp" 1
    revMix = ctrl "revMix" 0.5
    revRoom = ctrl "revRoom" 1.5
    attack = ctrl "attack" 0.5
