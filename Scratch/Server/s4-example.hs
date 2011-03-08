------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Example for S4.
--
import Control.Concurrent (threadDelay)
import Sound.OpenSoundControl
import Sound.SC3 hiding (pause)

import S4 (serveDefault)
import Client

main :: IO ()
main = do
  withSC3 $ \fd -> do
    async fd $ Bundle (NTPi 1)
      [d_recv $ synthdef "foo" (ug 0 3320 0.2)
      ,d_recv $ synthdef "bar" (ug 1 1330 0.3)]
  serveDefault [mkAct "foo" 0.8323, mkAct "bar" 0.7221]

mkAct def del = (def, act)
  where 
    act = do
      withSC3 $ \fd -> send fd $ s_new def (-1) AddToTail 1 []
      threadDelay $ floor $ del * 1e6

ug o f d = out o $ sinOsc ar f 0 * 0.3 * xLine kr 1 1e-9 d RemoveSynth