{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Testing functions and actions in Respond, take 2.
-}
module RespTest02 where

import Control.Applicative
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Respond hiding (setup)
import RespTest01 hiding (main,setup,loop01,loop02,loop03,loop04)
import qualified RespTest01 as RT01

main :: IO ()
main = w $ \fd -> do
  setup fd
  send fd $ s_new "rspdef3" 1003 AddToHead 1 []
  runMsg (ppar [loop01, loop02, loop03]) fd

setup :: Transport t => t -> IO OSC
setup fd = do
  RT01.setup fd
  async fd $ bundle immediately
    [d_recv $ synthdef "rspdef4" rspdef4
    ,d_recv $ synthdef "rspdef5" rspdef5]

rspdef4 :: UGen
rspdef4 = out 0 $
  lfPar AR ("freq"@@440 `lag` 0.25) 0 * ("amp"@@0.3 `lag3` 0.3)

-- | Variant of 'rspdef1', using 'in\'' ugen to map frequency factor.
rspdef5 :: UGen
rspdef5 =
  out 0 $ pan2
  (sinOsc AR ("freq"@@440 * (in' 1 KR ("fmul"@@100) `lag2` 3.5)) 0 *
   envGen kr ("t_trig"@@1) 0.3 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

loop01 :: Msg Double
loop01 = mkSnew AddToTail 1 "rspdef1"
  [("dur",

    -- pforever (1/17))
    pcycle [preplicate 1024 (1/23)
           ,preplicate 512 (2/23)
           ,preplicate 256 (4/23)
           ,preplicate 128 (8/23)])

  ,("freq", fmap midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  -- ,("fmul", pforever 100)
  ,("n_map/fmul", pforever 100)]

loop02 :: Msg Double
loop02 = mkSnew AddToTail 1 "rspdef2"
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp <$> prange (log <$> 110) (log <$> 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 :: Msg Double
loop03 = mkNset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

-- loop04 :: Transport t => t -> IO ()
-- loop04 = nSet AddToTail 1 "rspdef4"
--   [("dur", pcycle [1, 1/2, 1/4, 1/4, 1/2, 1/4, 1/4])
--   ,("freq", pforever $ prange 110 8800)
--   ,("amp",  pforever $ prange 1e-2 1)]

loop04 :: R (ToOSC Double)
loop04 = mkSnew AddToTail 1 "rspdef1"
  [("dur",  pforever $ prange 1e-3 7.5e-2)
  ,("freq", pforever $ exp <$> prange (log <$> 80) (log <$> 12000))
  ,("atk",  let xs = take 1024 $ iterate (*1.006) 0.002
            in  pcycle $ fmap pval (xs ++ reverse xs))
  ,("dcy",  pforever $ prange 1e-4 2e-1)
  ,("amp",  pforever $ prange 1e-1 3e-1)
  ,("pan",  pforever $ prange (-1) 1)]

msg01 :: Msg Double
msg01 = mkSnew AddToTail 1 "rspdef1"
  [("dur", pforever 20e-3)
  ,("freq", preplicate 200 8000)
  ,("atk", pforever 1e-3)
  ,("dcy", pforever 10e-3)
  ,("amp", pforever 0.3)]