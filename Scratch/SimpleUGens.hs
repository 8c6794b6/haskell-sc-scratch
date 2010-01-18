------------------------------------------------------------------------------
-- | Simple UGens.
-- 

module Scratch.SimpleUGens where

import Control.Monad (zipWithM)

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import Instances 

simpleUGens :: [(String,UGen)]
simpleUGens = 
    [("simplePercSine",simplePercSine),
     ("simpleReverb",simpleReverb),
     ("simplePanner",simplePanner),
     ("simpleTrigger",simpleTrigger)]
    

-- | Update ugens in this modules by writing to synthdef and reloading them.
updateSimpleUGens :: IO ()
updateSimpleUGens = do
  mapM_ (\(name,ugen) -> writeSynthdef name ugen) simpleUGens
  withSC3 reloadSynthdef
  -- writeSynthdef "simplePercSine" simplePercSine
  -- writeSynthdef "simpleReverb" simpleReverb
  -- writeSynthdef "simplePanner" simplePanner

-- | Simple UGen for making sound.
simplePercSine :: UGen
simplePercSine = out (control kr "out" 0) osc where
    osc = sinOsc ar freq 0 * amp
    freq = control kr "freq" 440
    amp = control kr "amp" (dbAmp (-20)) * env
    env = envGen kr envTrig 1 0 1 DoNothing envShape
    envTrig = control kr "trig" 0
    envShape = envCoord [(0,0),
                         (0.001,1),
                         (0.75,0.1),
                         (1,0)]
               (control kr "sustain" 0.8) 1 EnvExp
    -- envShape = envPerc 0.01 (control kr "sustain" 0.8)

-- | Simple reverb effect, using freeVerb.
simpleReverb :: UGen
simpleReverb = out (control kr "out" 0) result
    where result = freeVerb input mix room damp
          maxTime = 1.0
          input = in' 1 ar (control kr "in" 0)
          mix = control kr "mix" 0.5
          room = control kr "room" 0.5
          damp = control kr "damp" 0.5

-- | Simple panner.
simplePanner :: UGen
simplePanner = out 0 (pan2 input pan 1)
    where input = in' 1 ar (control kr "bus" 0)
          pan = control kr "pan" 0

-- | Simple trigger.
simpleTrigger :: UGen
simpleTrigger = out (control kr "out" 0) t
    where
      t = impulse kr (control kr "freq" 1) 0
                

pantest1 :: IO ()
pantest1 = do
  n <- pinkNoise ar
  audition $ out 0 $ pan2 n (fSinOsc kr 0.5 0) 0.3

pantest2 :: IO ()
pantest2 = do
  n <- pinkNoise ar  
  let x = mouseX kr (-1) 1 Linear 0.2
      y = mouseY kr 0 1 Linear 0.2
  audition $ out 0 $ pan2 n x y