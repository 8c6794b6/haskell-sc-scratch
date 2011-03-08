------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with beatTrack UGen.
--
module BeatTrackEx where

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

setup :: (Transport t) => t -> IO OSC
setup fd = do
  -- async fd $ Bundle immediately [b_free inBuf, b_free btBuf]
  async fd $ b_allocReadChannel inBuf inFile 0 0 [0,0]
  async fd $ b_alloc btBuf 1024 1 
  -- async fd $ b_read inBuf inFile 0 (-1) 0 1


inBuf :: (Num a) => a
inBuf = 1

inFile :: FilePath
inFile = "/home/atsuro/tmp/a006-capture3-3.wav"

btBuf :: (Num a) => a
btBuf = 10

bt1 :: UGen
bt1 = out 0 $ sig + i * 0.8
  where
    sig = mix $ sinOsc ar f 0 * a * decay2 (mce [b,h,q]) 5e-3 200e-3
    a = mce [0.4, 0.2, 0.1]
    f = mce [220, 880, 3300]
    MCE [b,h,q,_] = head $ mceProxies $ beatTrack (fft' btBuf i) x
    x = mouseX kr (-1) 1 Linear 0.2
    -- i = diskIn 2 inBuf Loop
    i = playBuf 2 inBuf (bufRateScale kr inBuf) 1 0 Loop DoNothing
