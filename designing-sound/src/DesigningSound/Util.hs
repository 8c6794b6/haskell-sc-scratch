------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Utility used in DesigningSound codes.
--
module DesigningSound.Util where

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
  send fd $ s_new name (-1) addAction target params
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Free the node from default scsynth.
nfree :: Int -> IO ()
nfree n = withSC3 $ \fd -> send fd $ n_free [n]

-- | Set the param in default scsynth.
nset :: Int -> [(String, Double)] -> IO ()
nset n ps = withSC3 $ \fd -> send fd $ n_set n ps

-- | Dump root node
dumpTree :: Transport t => (t -> IO ())
dumpTree fd = send fd $ Message "/g_dumpTree" [Int 0, Int 1]


------------------------------------------------------------------------------
--
-- UGen
--
------------------------------------------------------------------------------

-- | Select a sound from given ugen array.
selectX :: UGen -> UGen -> UGen
selectX i a = mkFilterMCE "SelectX" [i] a 1

-- | lag ud
lagUD :: UGen -> UGen -> UGen -> UGen
lagUD u d t = mkFilter "LagUD" [u, d, t] 1

------------------------------------------------------------------------------
--
-- For convinience
--
------------------------------------------------------------------------------

-- | Makes control for ugen, rate is always control rate.
-- When the control name starts with 't_', makes trigger control.
kcont :: String -> Double -> UGen
kcont name val
  | "t_" `isPrefixOf` name = tr_control name val
  | otherwise               = control kr name val
