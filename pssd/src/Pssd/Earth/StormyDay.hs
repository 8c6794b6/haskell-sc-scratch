------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- <http://obiwannabe.co.uk/tutorials/html/tutorial_thunder.html>
--
-- Demonstration of making stormy day sound with using synthdefs from other
-- modules.
--
-- Try:
--
-- > > stormyDay
--
module Pssd.Earth.StormyDay where

import Sound.SC3
import Sound.SC3.Lepton

import Pssd.Earth.Bubbles
import Pssd.Earth.Rain
import Pssd.Earth.Stream
import Pssd.Earth.Thunder
import Pssd.Util

-- | Play the sound.
stormyDay :: IO ()
stormyDay = do
  withSC3 $ \fd -> do
    let dr (n, u) = async fd . d_recv $ synthdef n u
    mapM_ dr [("wind",wind)
             ,("rain",rain)
             ,("bTrig",bTrig)
             ,("bolt2",bolt2)
             ,("afterImages",afterImages)
             ,("boom",boom)]
    addNode 0 graph fd

-- | Synthdef graph.
graph :: SCNode
graph =
  Group 0
    [Group 1
      [Group 10
        [Synth 1001 "bTrig" ["out":=fromIntegral tBus ,"freq":=0.125]]
      ,Group 20
        [Synth 2001 "wind" ["amp":=0.2]
        ,Synth 2002 "rain" ["amp":=1.0]
        ,Synth 2003 "bolt2" ["t_trig":<-tBus]
        ,Synth 2004 "afterImages" ["t_trig":<-tBus]
        ,Synth 2005 "boom" ["t_trig":<-tBus]]]]
  where
    tBus = 101
