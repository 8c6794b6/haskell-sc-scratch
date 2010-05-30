------------------------------------------------------------------------------
-- |
-- Scratch to dump incoming messages.
-- 

module Dumper where

import Sound.SC3.Wing.MIDI

main :: IO () 
main = withMIDI "Dumper" print