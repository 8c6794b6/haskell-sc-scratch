module ImportMe where

import Sound.SC3

foo :: UGen
foo = out 0 (sinOsc ar 440 0 * 0.1)