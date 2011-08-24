{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Sequential OSC message sending without Control.Concurrent.threadDelay.

-}
module Respond where

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Unique
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import qualified Data.Map as M
import qualified Data.Traversable as T

-- | Wraps sending @notify True@ and @notify False@ messages before and
-- after 'withSC3'.
w :: (UDP -> IO a) -> IO a
w k = withSC3 $ \fd -> bracket
  (async fd (notify True) >> return fd)
  (\fd' -> send fd' (notify False))
  k

-- | Synthdef used for responding to 'n_set' messages.
--
-- Contains 'sendTrig' UGen.
tr :: UGen
tr = sendTrig ("t_trig"@@0) ("id"@@0) ("val"@@0)

setup :: Transport t => t -> IO OSC
setup fd = async fd $ d_recv $ synthdef "tr" tr

offsetDelay :: Double
offsetDelay = 0.1

newNid :: IO Int
newNid = (+ 10000) . hashUnique <$> newUnique

newNidInBetween :: Int -> Int -> IO Int
newNidInBetween a b = do
  uq <- newUnique
  return $ a + ((hashUnique uq) `mod` (b-a))

waitUntil :: Transport t => t -> String -> Int -> IO ()
waitUntil fd str n = recv fd >>= \m -> case m of
  Message str' (Int n':_) | str == str' && n == n' -> return ()
  _                       -> waitUntil fd str n
{-# SPECIALISE waitUntil :: UDP -> String -> Int -> IO () #-}
{-# SPECIALISE waitUntil :: TCP -> String -> Int -> IO () #-}

-- | Send 's_new' message using pattern.
--
sNew :: Transport t
  => AddAction
  -- ^ Add action for s_new message
  -> Int
  -- ^ Node id of add target
  -> String
  -- ^ Synthdef name
  -> [(String,R Double)]
  -- ^ Param name and pattern for the param, passed to 'mkOpts'.
  -> t -> IO ()
sNew aa tid def pms fd = join $ foldM_ f <$> utcr <*> k pms where
  k = runPIO . T.sequenceA . M.fromList
  f t0 m = do
    nid <- newNid
    let dt = M.findWithDefault 1 "dur" m
        (opts,ps) = M.partitionWithKey (\k _ -> '/' `elem` k) m
    send fd $ bundle (UTCr $ t0+dt+offsetDelay) $
      s_new def nid aa tid (M.assocs ps) : M.foldrWithKey (mkOpts nid) [] opts
    waitUntil fd "/n_go" nid
    return (t0+dt)
{-# SPECIALISE sNew ::
   AddAction -> Int -> String -> [(String,R Double)] -> UDP -> IO () #-}
{-# SPECIALISE sNew ::
   AddAction -> Int -> String -> [(String,R Double)] -> TCP -> IO () #-}

-- | Make OSC message from given name.
--    
-- [@n_set/PARAM_NAME@] @n_set@ message will be sent to @PARAM_NAME@
-- parameter of newly created node.
--
-- [@n_map/PARAM_NAME@] @n_map@ message will be sent to @PARAM_NAME@ 
-- parameter of newly created node. 
--
-- [@n_mapa/PARAM_NAME@] @n_mapa@ message will be sent to @PARAM_NAME@ 
-- parameter of newly created node.
--
-- [@c_set/BUSID@] @c_set@ message will sent to bus which its id is BUSID. 
--     
mkOpts ::
  Int
  -- ^ Node id
  -> String
  -- ^ Key name
  -> Double
  -- ^ Value
  -> [OSC]
  -- ^ List of OSC used as accumulator
  -> [OSC]
mkOpts nid a val acc = case break (== '/') a of
  (k,v) -> case (k,v) of
    ("n_set",'/':p)  -> n_set nid [(p,val)]:acc
    ("n_map",'/':p)  -> n_map nid [(p,ceiling val)]:acc
    ("n_mapa",'/':p) -> n_mapa nid [(p,ceiling val)]:acc
    ("c_set", '/':p) -> c_set [(read p, val)]:acc
    _                -> acc

-- | Send 'n_set' message using pattern.
nSet :: Transport t 
  => AddAction 
  -- ^ Add action
  -> Int 
  -- ^ Target node id of AddAction 
  -> String
  -- ^ Synthdef name
  -> [(String,R Double)] 
  -- ^ Pair of parameter and value
  -> t -> IO ()
nSet aa tid def pms fd = do
  -- XXX: Add gate releasing control with 'legato' value in map.
  nid  <- newNid
  trid <- newNid
  send fd $ bundle immediately
    [s_new def nid aa tid [],s_new "tr" trid AddBefore nid []]
  join $ foldM_ (f nid trid) <$> utcr <*> k pms
  where
    k = runPIO . T.sequenceA . M.fromList
    f nid trid t0 m = do
      let dt = M.findWithDefault 1 "dur" m
      send fd $ bundle (UTCr $ t0+dt+offsetDelay)
        [n_set nid (M.assocs m),n_set trid [("t_trig",1)]]
      waitUntil fd "/tr" trid
      return (t0+dt)
{-# SPECIALISE nSet ::
   AddAction -> Int -> String -> [(String,R Double)] -> UDP -> IO () #-}
{-# SPECIALISE nSet ::
   AddAction -> Int -> String -> [(String,R Double)] -> TCP -> IO () #-}
