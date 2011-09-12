{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
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

import Control.Applicative
import Data.Data
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

import Control.DeepSeq
import Data.Binary (Binary)
import Data.Serialize (Serialize)
import Sound.OpenSoundControl
import Sound.SC3 hiding (Binary)

import Sound.SC3.Lepton.Tree.Tree

import qualified Data.Binary as B
import qualified Data.Map as M
import qualified Data.Serialize as S

-- | OSC convertable data
data ToOSC a = ToOSC
  { -- | Type of OSC message.
    oscType :: MsgType
    -- | Arguments for OSC message.
  , oscMap  :: M.Map String a
  } deriving (Eq, Show, Read, Data, Typeable)

instance Functor ToOSC where
  fmap f (ToOSC t m) = ToOSC t (fmap f m)

-- | Message type is left associative, (+), (*), (-) will union given maps.
instance Num a => Num (ToOSC a) where
  ToOSC o m1 + ToOSC _ m2 = ToOSC o (M.unionWith (+) m1 m2)
  ToOSC o m1 * ToOSC _ m2 = ToOSC o (M.unionWith (*) m1 m2)
  ToOSC o m1 - ToOSC _ m2 = ToOSC o (M.unionWith (-) m1 m2)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger d = ToOSC o m where
    o = Snew "silence" Nothing AddToTail 1
    m = M.singleton "dur" (fromInteger d)

instance Fractional a => Fractional (ToOSC a) where
  ToOSC o m1 / ToOSC _ m2 = ToOSC o (M.unionWith (/) m1 m2)
  recip = fmap recip
  fromRational a = ToOSC o m where
    o = Snew "silence" Nothing AddToTail 1
    m = M.singleton "dur" (fromRational a)

instance Ord a => Ord (ToOSC a) where
  compare (ToOSC _ m1) (ToOSC _ m2) = compare m1 m2

instance Floating a => Floating (ToOSC a) where
  pi = ToOSC o m where
    o = Snew "silence" Nothing AddToTail 1
    m = M.singleton "dur" pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (ToOSC o m1) ** (ToOSC _ m2) = ToOSC o (M.unionWith (**) m1 m2)
  logBase (ToOSC o m1) (ToOSC _ m2) = ToOSC o (M.unionWith logBase m1 m2)
  sin  = fmap sin
  tan  = fmap tan
  cos  = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  tanh  = fmap tanh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance UnaryOp a => UnaryOp (ToOSC a)

instance Serialize a => Serialize (ToOSC a) where
  {-# INLINE put #-}
  put (ToOSC t m) = S.put t >> S.put m
  {-# INLINE get #-}
  get = ToOSC <$> S.get <*> S.get

instance Binary a => Binary (ToOSC a) where
  {-# INLINE put #-}
  put (ToOSC t m) = B.put t >> B.put m
  {-# INLINE get #-}
  get = ToOSC <$> B.get <*> B.get

instance NFData a => NFData (ToOSC a) where
  rnf (ToOSC m ps) = rnf m `seq` rnf ps `seq` ()

-- | Type of OSC message.
data MsgType
  = Snew String (Maybe NodeId) AddAction NodeId
  | Nset NodeId
  deriving (Eq, Show, Read, Data, Typeable)

instance NFData MsgType where
  rnf (Snew d n a t) = rnf d `seq` rnf n `seq` rnf a `seq` rnf t `seq` ()
  rnf (Nset t) = rnf t `seq` ()

instance NFData AddAction

instance Serialize MsgType where
  {-# INLINE put #-}
  put m = case m of
    Snew d n a t -> S.putWord8 0 *> S.put d *> S.put n *> S.put a *> S.put t
    Nset t       -> S.putWord8 1 *> S.put t
  {-# INLINE get #-}
  get = S.getWord8 >>= \i -> case i of
    0 -> Snew <$> S.get <*> S.get <*> S.get <*> S.get
    1 -> Nset <$> S.get
    n -> error $ "Unexpected index in get: " ++ show n

instance Binary MsgType where
  {-# INLINE put #-}
  put m = case m of
    Snew d n a t -> B.putWord8 0 *> B.put d *> B.put n *> B.put a *> B.put t
    Nset t       -> B.putWord8 1 *> B.put t
  {-# INLINE get #-}
  get = B.getWord8 >>= \i -> case i of
    0 -> Snew <$> B.get <*> B.get <*> B.get <*> B.get
    1 -> Nset <$> B.get
    n -> error $ "Unexpected index in get: " ++ show n

instance Read AddAction where
  readsPrec _ s = readAddAction s

readAddAction :: ReadS AddAction
readAddAction s = case lex s of
  [("AddToHead",s')]  -> [(AddToHead,s')]
  [("AddToTail",s')]  -> [(AddToTail,s')]
  [("AddBefore",s')]  -> [(AddBefore,s')]
  [("AddAfter",s')]   -> [(AddAfter,s')]
  [("AddReplace",s')] -> [(AddReplace,s')]
  _                   -> []
{-# INLINE readAddAction #-}

instance Serialize AddAction where
  {-# INLINE put #-}
  put m = case m of
    AddToHead  -> S.putWord8 0
    AddToTail  -> S.putWord8 1
    AddBefore  -> S.putWord8 2
    AddAfter   -> S.putWord8 3
    AddReplace -> S.putWord8 4
  {-# INLINE get #-}
  get = S.getWord8 >>= \i -> case i of
    0 -> return AddToHead
    1 -> return AddToTail
    2 -> return AddBefore
    3 -> return AddAfter
    4 -> return AddReplace
    n -> error $ "Unexpected index in get: " ++ show n

instance Binary AddAction where
  {-# INLINE put #-}
  put m = case m of
    AddToHead  -> B.putWord8 0
    AddToTail  -> B.putWord8 1
    AddBefore  -> B.putWord8 2
    AddAfter   -> B.putWord8 3
    AddReplace -> B.putWord8 4
  {-# INLINE get #-}
  get = B.getWord8 >>= \i -> case i of
    0 -> return AddToHead
    1 -> return AddToTail
    2 -> return AddBefore
    3 -> return AddAfter
    4 -> return AddReplace
    n -> error $ "Unexpected index in get: " ++ show n

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

class Show a => Cue a where
  cueDur   :: a -> Double
  asOSC    :: a -> OSC
  setCueId :: Int -> a -> a
  getCueId :: a -> Int
  isRest   :: a -> Bool

data Sn = Sn String (Maybe Int) AddAction Int (M.Map String Double)
        deriving (Eq,Show)
data Ns = Ns Int (M.Map String Double)
        deriving (Eq,Show)

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

data Event where
  Event :: forall e. Cue e => e -> Event

instance Cue Event where
  asOSC (Event n) = asOSC n
  isRest (Event n) = isRest n
  cueDur (Event n) = cueDur n
  getCueId (Event n) = getCueId n
  setCueId i (Event n) = Event (setCueId i n)

instance Show Event where
  show (Event n) = shows ("Event: " ++ show n) ""