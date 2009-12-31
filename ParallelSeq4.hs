----------------------------------------------------------------------
-- | Parallel sequence studying, take4.
--
-- This time using @n_map@ to control triggers and parameters of ugen
-- which making the sound.
--

module ParallelSeq4 where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import SimpleNotes

-- | Simple UGen for making sound.
para4UGen :: UGen
para4UGen = out 0 (pan2 osc pos 1) where
    osc = sinOsc ar freq 0 * amp
    pos = control kr "pan" 0.0
    freq = control kr "freq" 440
    amp = control kr "amp" (dbAmp 80) * env
    env = envGen kr envTrig 1 0 1 DoNothing envShape
    envTrig = control kr "trig" 0
    envShape = envPerc 0.01 0.8

-- | UGen to send trigger.
trigUGen :: UGen
trigUGen = undefined

-- | UGen to send parameters.
paramUGen :: UGen
paramUGen = undefined

-- | Setup mappings and sound making ugens.
setup :: IO ()
setup = undefined

-- | Go with player ugen.
go :: IO ()
go = undefined
