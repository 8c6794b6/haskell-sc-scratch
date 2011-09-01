{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (FlexibleInstances)

Types for OSC messages.

-}
module Sound.SC3.Lepton.Pattern.ToOSC where

import Data.Maybe (fromMaybe)

import Sound.OpenSoundControl
import Sound.SC3

import Sound.SC3.Lepton.Tree.Tree

import qualified Data.Map as M

-- | OSC convertable data
data ToOSC a = ToOSC
  { -- | Type of OSC message.
    oscType :: MsgType
    -- | Arguments for OSC message.
  , oscMap  :: M.Map String a
  } deriving (Eq, Show)

instance Functor ToOSC where
  fmap f (ToOSC t m) = ToOSC t (fmap f m)

-- | Type of OSC message.
data MsgType
  = Snew String (Maybe NodeId) AddAction NodeId
  | Nset NodeId
  deriving (Eq, Show)

-- | Converts to OSC messages.
toOSC :: ToOSC Double -> OSC
toOSC (ToOSC t m) = case t of
  Snew def nid aa tid -> s_new def (fromMaybe (-1) nid) aa tid (M.assocs m)
  Nset nid            -> n_set nid (M.assocs m)

-- | Update NodeId of ToOSC.
setNid :: NodeId -> ToOSC a -> ToOSC a
setNid nid o = case oscType o of
  Snew def _ aa tid -> ToOSC (Snew def (Just nid) aa tid) (oscMap o)
  Nset _            -> ToOSC (Nset nid) (oscMap o)

-- | Get duration of ToOSC.
--
-- Expecting duration defined as length of time until the next event.
--
-- This function will lookup for \"dur\" key in oscMap of ToOSC.
-- When there's no value, returns 1.
--
getDur :: Num a => ToOSC a -> a
getDur o = fromMaybe 1 $ M.lookup "dur" (oscMap o)
{-# SPECIALIZE getDur :: ToOSC Double -> Double #-}

-- | Modify contents of specified key with given function
tadjust :: String -> (a -> a) -> ToOSC a -> ToOSC a
tadjust k f (ToOSC ot om) = ToOSC ot (M.adjust f k om)

-- | Modify contents of specified key with given function, via fmap.
madjust :: Functor f => String -> (a -> a) -> f (ToOSC a) -> f (ToOSC a)
madjust k f r = fmap (tadjust k f) r
{-# INLINEABLE madjust #-}

-- ---------------------------------------------------------------------------
-- Classes and instances

-- class Event e where
--   type Evalue e :: *
--   eOSC :: e (Evalue e) -> OSC
--   eDur :: e (Evalue e) -> Double

-- instance Event ToOSC where
--   type Evalue ToOSC = Double
--   eOSC = toOSC
--   eDur = getDur

class Cue a where
  cueDur   :: a -> Double
  asOSC    :: a -> OSC
  setCueId :: Int -> a -> a
  getCueId :: a -> Int
  isRest   :: a -> Bool

data Sn = Sn String (Maybe Int) AddAction Int (M.Map String Double)
data Ns = Ns Int (M.Map String Double)

instance Cue Sn where
  cueDur (Sn _ _ _ _ m) = M.findWithDefault 1 "dur" m
  asOSC (Sn def nid aa tid m) = s_new def (fromMaybe (-1) nid) aa tid (M.assocs m)
  setCueId nid (Sn def _ aa tid m) = Sn def (Just nid) aa tid m
  getCueId (Sn _ nid _ _ _) = fromMaybe (-1) nid
  isRest (Sn _ _ _ _ m) = maybe False zeroOrNaN $ M.lookup "freq" m

instance Cue Ns where
  cueDur (Ns _ m) = M.findWithDefault 1 "dur" m
  asOSC  (Ns i m) = n_set i (M.assocs m)
  setCueId i (Ns _ m) = Ns i m
  getCueId (Ns i _) = i
  isRest (Ns _ m) = maybe False zeroOrNaN $ M.lookup "freq" m

instance Cue (ToOSC Double) where
  cueDur = getDur
  asOSC o = toOSC o
  setCueId = setNid
  getCueId = undefined
  isRest (ToOSC _ m) = maybe False zeroOrNaN $ M.lookup "freq" m

zeroOrNaN :: Double -> Bool
zeroOrNaN x = isNaN x || x == 0