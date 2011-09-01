{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

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
