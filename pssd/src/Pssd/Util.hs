------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Utility used by others.
--
module Pssd.Util where

import Data.List (isPrefixOf)
import Sound.SC3
import Sound.OpenSoundControl

------------------------------------------------------------------------------
--
-- Server communication
--
------------------------------------------------------------------------------

-- | Send new synth with specifying synthdef name.
-- Return newly created negative node id.
audit :: String -> UGen -> IO Int
audit name ug = withSC3 $ \fd -> do
  let name' | null name = "anon"
            | otherwise = name
  send fd . d_recv $ synthdef name' ug
  _ <- wait fd "/done"
  send fd $ s_new name' (-1) AddToTail 1 []
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Send new synth, return newly created negative node id.
anon :: UGen -> IO Int
anon ug = audit "" ug

anonymous :: (Transport t) => t -> UGen -> IO Int
anonymous fd ug = do
  send fd $ d_recv $ synthdef "anon" ug
  _ <- wait fd "/done"
  send fd $ s_new "anon" (-1) AddToTail 1 []
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Send new synthdef.
drecv :: String -> UGen -> IO OSC
drecv name ug = withSC3 $ \fd -> async fd . d_recv $ synthdef name ug

-- | add new synth bo default scsynth, specified by name and params.
snew :: String -> [(String, Double)] -> IO Int
snew name ps = withSC3 $ \fd -> do
  send fd $ s_new name (-1) AddToTail 1 ps
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

s_new_id :: (Transport t)
         => t
         -> String
         -> AddAction
         -> Int
         -> [(String, Double)]
         -> IO Int
s_new_id fd name addAction target params = do
  send fd $ Bundle (UTCr 0) [ s_new name (-1) addAction target params
                            , s_get (-1) [] ]
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Free the node from default scsynth.
nfree :: Int -> IO ()
nfree n = withSC3 $ \fd -> send fd $ n_free [n]

-- | Set the param in default scsynth.
nset :: Int -> [(String, Double)] -> IO ()
nset n ps = withSC3 $ \fd -> send fd $ n_set n ps

-- | Async d_recv with (name, ugen) tuple.
adrcv :: (Transport t) => t -> (String,UGen) -> IO OSC
adrcv fd (n,u) = async fd $ d_recv $ synthdef n u


------------------------------------------------------------------------------
--
-- UGen
--
------------------------------------------------------------------------------

-- | Attack, sustain, release envelope parameter constructor.
envASR :: UGen -> UGen -> UGen -> EnvCurve -> [UGen]
envASR aT sL rT c =
    let l = [0,sL,0]
        t = [aT,rT]
        c' = [c,c]
    in env l t c' 1 (-1)

-- | Select a sound from given ugen array.
selectX :: UGen -> UGen -> UGen
selectX i a = mkFilterMCE "SelectX" [i] a 1

-- | lag ud
lagUD :: UGen -> UGen -> UGen -> UGen
lagUD u d t = mkFilter "LagUD" [u, d, t] 1

-- | Differential
differential :: UGen -> UGen
differential n = x1 - x0 / y1 - y0
  where
    x0 = sos n 0 0 1 0 0
    x1 = sos n 0 1 0 0 0
    y0 = sos n 0 0 0 1 0
    y1 = sos n 0 0 0 0 1

-- | Stereo output starting from channel 0.
--
-- > out2 == out 0 . mce . replicate 2
--
out2 :: UGen -> UGen
out2 = out 0 . mce . replicate 2
