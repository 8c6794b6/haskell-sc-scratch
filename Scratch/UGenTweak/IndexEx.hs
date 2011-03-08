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
module IndexEx where

import Sound.SC3
import Sound.SC3.Lepton

buf1 = 1

setupBuf fs = withSC3 $ \fd -> do
  async fd $ b_alloc buf1 1 6
  send fd $ b_setn buf1 [(0,fs)]
  
-- setupBuf [330,440,550,660,770,880]
-- setupBuf [110,330,220,550,440,770]
-- setupBuf [660,330,220,880,440,550]
  
i1 = out 0 $ sinOsc ar 
     (index (asLocalBuf 'a' [330,440,550,660,770,880])
      (linLin (lfSaw kr 1 0) (-1) 1 0 6)) 0  * 0.3
     
i2 = out 0 $ sinOsc ar 
     (index (fromIntegral buf1)
      (linLin (lfSaw kr 1 0) (-1) 1 0 6)) 0  * 0.3

i3 = out 0 $ sinOsc ar 
     (index (fromIntegral buf1) ("idx"=:0)) 0 * 0.3

