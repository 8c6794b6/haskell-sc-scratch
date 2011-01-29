------------------------------------------------------------------------------
-- | Example for using ReplaceOut UGen.
--

module Scratch.ReplaceOutEx where

import Sound.SC3
import Sound.OpenSoundControl

import Reusable
import Instances

a,b,c,d :: UGen
a = out 0 $ sinOsc ar (mce [330,331]) 0 * 0.1
b = out 0 $ sinOsc ar (mce [880,881]) 0 * 0.1
c = out 0 $ sinOsc ar (mce [120,121]) 0 * 0.1
d = replaceOut 0 $ sinOsc ar (mce [880,881]) 0 * 0.1
e = out 0 $ saw ar (mce [440,441]) * 0.1
f = out 0 $ pulse ar (mce [660,661]) 0.5 * 0.1

test1,test2 :: IO ()
test1 = audition (mrg [d,f,e])
test2 = audition (mrg [f,b,e])
