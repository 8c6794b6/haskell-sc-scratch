------------------------------------------------------------------------------
-- | Example for LocalIn and LocalOut UGens.
--

module LocalInEx where

import Sound.OpenSoundControl
import Sound.SC3

import Reusable
import Instances

test1 :: (UId m) => m UGen
test1 = do
  n <- whiteNoise ar
  let a0 = decay (impulse ar 0.3 0) 0.1 * n * 0.2
      a1 = localIn 2 ar + mce [a0,0]
      a2 = delayN a1 0.2 0.2
      a3 = mceEdit reverse a2 * 0.8
  return $ mrg [localOut a3, out 0 a2]

test2 :: (UId m) => m UGen
test2 = return $ mrg [offsetOut 0 p, localOut d] 
    where p = localIn 1 ar
          i = impulse ar 1 0
          d = delayC (i+(p*0.995)) 1 (recip 440 - recip controlRate)

-- test for offsetOut
test3 = audition $ mrg [a,b] 
    where a = offsetOut 0 (impulse ar 5 0)
          b = out 0 (sinOsc ar 60 0 * 0.1)

test4 = audition $ mrg [a,b]
    where a = out 0 $ impulse ar 5 0
          b = out 0 $ sinOsc ar 60 0 * 0.1

