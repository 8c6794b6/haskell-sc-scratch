{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (FlexibleInstances, BangPatterns)

Sends OSC message sequentially with responding to server.
-}
module Sound.SC3.Lepton.Pattern.Play where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.IO (IOMode(..), withFile)

import Data.Unique
import Sound.OSC hiding (waitUntil)
import Sound.OSC.Coding.Byte
import Sound.SC3
import Sound.SC3.Tree

import Sound.SC3.Lepton.Pattern.Interpreter.L
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.UGen.Factory


import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Types

-- | Type class used to play sequence of message.
class Playable p where
  playIO :: Transport m => (b -> a -> m b) -> b -> p a -> m ()

-- | Plays pattern.
--
-- A synthdef containing sendTrig ugen will be sent to server for each
-- invokation of play action to pattern. When this synth has been removed, pattern
-- sequence will stop.
--
-- instance (Playable p) => Audible (p (ToOSC Double)) where
--   play r = liftIO $ bracket
--     (do send (notify True)
--         trid <- liftIO newNid
--         return trid)
--     (\trid ->
--       sendOSC (bundle immediately [notify False, n_free [trid]]))
--     (\trid -> runMsg r trid)

-- playL r = bracket
--         (do send (notify True)
--             trid <- liftIO newNid
--             return trid)
--         (\trid ->
--           sendOSC (bundle immediately [notify False, n_free [trid]]))
--         (\trid -> runMsg r trid)

instance Playable p => Audible (p (ToOSC Double)) where
    play r = withNotifications $ do
        trid <- liftIO newNid
        runMsg r trid
        sendOSC $ n_free [trid]

------------------------------------------------------------------------------
-- Guts

instance Playable (L ()) where
  playIO = foldLIO_

-- | Run message immediately
runMsg :: (Playable p, Transport m)
  => p (ToOSC Double)
  -- ^ Pattern to play
  -> Int
  -- ^ Node id used for trigger synth
  -> m ()
runMsg msg trid = time >>= \now -> runMsgFrom now msg trid
{-# SPECIALISE runMsg :: L () (ToOSC Double) -> Int -> Connection UDP () #-}
{-# SPECIALISE runMsg :: L () (ToOSC Double) -> Int -> Connection TCP () #-}

-- | Run message from given time.
--
-- When 'NaN' is found in 'freq' key of Map, replace the message
-- with sending 's_new silence'.
--
runMsgFrom
  :: (Playable p, Transport m) =>
     Time -> p (ToOSC Double) -> Int -> m ()
--
-- XXX: Not using bracket due to constraint in Audible.
-- runMsgFrom now msg trid = bracket_ newTrigger freeTrigger work where
--
runMsgFrom now msg trid = newTrigger >> work >> freeTrigger where
  --
  -- When delta time is small amount, latency (> 1ms) occurs in
  -- server side. Sending messages in chunk, accumulate until enough
  -- duration has been passed in betweenmessages.
  --
  -- XXX:
  -- Try pattern converter and sender running in different thread, with
  -- passing around chunks via MVar. This may save couple computation time.
  --
  newTrigger = send $ s_new "tr" trid AddToHead 1 []
  freeTrigger = send $ n_free [trid]
  work = playIO go (now,now,[]) msg
  go (t0,t1,acc) o
    | getDur o == 0 || null acc = return (t0,t1,o:acc)
    | otherwise                 = do
      let dt = getDur o
          enoughTime = (t0+dt)-t1 > (offsetDelay*5)
      msgs <- mkOSCs acc trid
      sendOSC $ bundle (t0+offsetDelay) (tick:msgs)
      t1' <- if enoughTime then time else return t1
      when enoughTime $ waitUntil "/tr" trid
      return (t0+dt,t1',[o])
  tick = n_set trid [("t_trig",1)]
{-# SPECIALISE runMsgFrom ::
    Time -> L () (ToOSC Double) -> Int -> Connection TCP () #-}
{-# SPECIALISE runMsgFrom ::
    Time -> L () (ToOSC Double) -> Int -> Connection UDP () #-}

-- | Run pausable message. MVar contains the time assumed as now.
--
-- This action is checking contents of MVar for every response.
-- For performance significant message sending, use runMsg or runMsgFrom.
--
-- runPausableMsg :: (Playable p, MonadCatch m, MonadIO m, DuplexOSC m)
--   => MVar Double -> p (ToOSC Double) -> Int -> m ()
runPausableMsg ::
  (Playable p, Transport m)
  => MVar Double -> p (ToOSC Double) -> Int -> m ()
-- runPausableMsg mvar msg trid = bracket_ newTrig freeTrig work where
runPausableMsg mvar msg trid = work where
  -- newTrig = send $ s_new "tr" trid AddToHead 1 []
  -- freeTrig = send $ n_free [trid]
  -- work = foldPIO_ go [] msg
  work = playIO go [] msg
  go acc o
    | getDur o == 0 || null acc = return (o:acc)
    | otherwise                 = do
      -- XXX: When to get and put contents of MVar?
      -- Is this use of MVar thread safe?
      let dt = getDur o
      msgs <- liftIO $ mkOSCs acc trid
      -- liftIO $ modifyMVar_ mvar $ \tl -> case tl + dt of
      --   tl' -> mapM_ sendOSC [ bundle tl' [tick]
      --                        , bundle (tl'+offsetDelay) msgs ] >>
      --          return tl'
      os <- liftIO $ modifyMVar mvar $ \tl ->
        let tl' = tl + dt
        in  return (tl', [bundle tl' [tick], bundle (tl'+offsetDelay) msgs])
      mapM_ sendOSC os
      waitUntil "/tr" trid
      return [o]
  tick = n_set trid [("t_trig",1)]

mkOSCs :: MonadIO m => [ToOSC Double] -> Int -> m [Message]
mkOSCs os tid = foldM f [] os where
  f acc o
    | isSilence o = do
      nid <- liftIO newNid
      let slt = Snew "rest" (Just nid) AddToTail 1
          rest = toOSC (ToOSC slt (M.singleton "dur" (getDur o)))
      slt `seq` rest `seq` return (rest:acc)
    | otherwise   = case oscType o of
      Snew _ nid _ _ -> do
        nid' <- liftIO $ maybe newNid return nid
        let ops = M.foldrWithKey (mkOpts nid') [] (oscMap o)
            o' = toOSC (setNid nid' o)
        o' `seq` ops `seq` return ((toOSC (setNid nid' o):ops) ++ acc)
      Nset _ -> do
        let t = ToOSC (Nset tid) (M.singleton "t_trig" 1)
            o' = toOSC o
        o' `seq` t `seq` return (o':toOSC t:acc)

-- | Synthdef used for responding to 'n_set' messages.
--
-- Contains 'sendTrig' UGen.
tr :: UGen
tr = sendTrig ("t_trig"@@0) ("id"@@0) ("val"@@0)

-- | Silent UGen with done action.
silenceS :: UGen
silenceS = freeSelf (impulse KR 1 0)

-- | Sends synthdefs used by pattern player.
setup :: DuplexOSC m => m ()
setup = mapM_ async
  [d_recv $ synthdef "tr" tr
  ,d_recv $ synthdef "silence" silenceS
  ,d_recv $ synthdef "rest" silenceS
  ,d_recv $ synthdef "done" silenceS]

offsetDelay :: Double
offsetDelay = 0.1

-- | Return unique id, starting from 10000.
newNid :: IO Int
newNid = (+ 10000) . hashUnique <$> newUnique

-- | Return Int value, cycling between given two values.
newNidInBetween :: Int -> Int -> IO Int
newNidInBetween a b = do
  uq <- newUnique
  return $ a + ((hashUnique uq) `mod` (b-a))

-- | Wait until specified message has been returned from server.
waitUntil ::
  (RecvOSC m, MonadIO m)
  => String
  -- ^ String to match in returned OSC message.
  -> Int
  -- ^ Int to match in first element of returned OSC message.
  -> m ()
waitUntil str n = recvMessage >>= \m -> case m of
  Just (Message !str' (Int32 !n':_))
      | str == str' && n == fromIntegral n' -> return ()
  _                                         -> waitUntil str n
{-# SPECIALISE waitUntil :: String -> Int -> Connection UDP () #-}
{-# SPECIALISE waitUntil :: String -> Int -> Connection TCP () #-}

{-
waitUntil2 ::
  Transport t
  => t
  -> String
  -- ^ String to match in returned OSC message.
  -> Int
  -- ^ Int to match in first element of returned OSC message.
  -> Int
  -- ^ Trigger id to match
  -> IO ()
waitUntil2 fd str n i = recv fd >>= \m -> case m of
  Message !str' (Int32 (!n'):Int32 (!i'):_)
    | str == str' && n == n' && i == i' -> return ()
  _                                     -> waitUntil2 fd str n i
-}

-- | Make OSC message from given name.
--
-- [@n_set/PARAM_NAME@] @n_set@ message with @PARAM_NAME@ used as parameter.
--
-- [@n_map/PARAM_NAME@] @n_map@ message with @PARAM_NAME@ used as parameter.
--
-- [@n_mapa/PARAM_NAME@] @n_mapa@ message with @PARAM_NAME@ used as parameter.
--
-- [@c_set/BUSID@] @c_set@ message with BUSID as bus id.
--
mkOpts ::
  Int
  -- ^ Node id
  -> String
  -- ^ Key name
  -> Double
  -- ^ Value in result message.
  -> [Message]
  -- ^ List of OSC used as accumulator
  -> [Message]
mkOpts nid a val acc = case break (== '/') a of
  (k,v) -> case (k,v) of
    ("n_set",'/':p)  -> n_set nid [(p,val)]:acc
    ("n_map",'/':p)  -> n_map nid [(p,ceiling val)]:acc
    ("n_mapa",'/':p) -> n_mapa nid [(p,ceiling val)]:acc
    ("c_set", '/':p) -> c_set [(read p, val)]:acc
    _                -> acc

------------------------------------------------------------------------------
-- Non-realtime

-- | Write OSC from pattern, for non-realtime use with scsynth.
writeScore :: (Playable p) =>
  [Message]
  -- ^ Initial OSC message.
  -> SCNode
  -- ^ Initial node graph.
  -> L () (ToOSC Double)
  -- ^ Pattern containing OSC message.
  -> FilePath
  -- ^ Path to save OSC data.
  -> IO ()
writeScore ini n0 pat path = withFile path WriteMode $ \hdl -> do
  let n0' = diffMessage (Group 0 []) n0
  -- BSL.hPut hdl (oscAndSize (bundle (NTPr 0) (n0' ++ ini)))
  BSL.hPut hdl (oscAndSize (bundle 0 (n0' ++ ini)))
  foldLIO_ (k hdl) 0 pat
  where
    k hdl t o
      | isSilence o = return (t+getDur o)
      | otherwise   = do
        o' <- case oscType o of
          Nset _ -> return $ [toOSC o]
          Snew _ nid _ _ -> do
            nid' <- maybe newNid return nid
            let opts = M.foldrWithKey (mkOpts nid') [] (oscMap o)
            return $ toOSC (setNid nid' o):opts
        let t' = t + getDur o
        -- BSL.hPut hdl (oscAndSize $ bundle (NTPr t') o')
        liftIO $ BSL.hPut hdl (oscAndSize $ bundle t' o')
        return t'
    oscAndSize o = BSL.append l b where
      b = encodeOSC o
      l = encode_i32 (fromIntegral (BSL.length b))

-- | Root node with a group with node id 1.
defaultGroup :: SCNode
defaultGroup = Group 0 [Group 1 []]

-------------------------------------------------------------------------------
-- Debugging

-- dumpMsg :: Num a => Msg a -> IO ()
-- dumpMsg = dumpMsgWith id

-- -- dumpMsgWith :: Num a => ([p] -> [ToOSC a]) -> R p -> IO ()
-- dumpMsgWith f m =
--   S.mapM_ (\os -> putStrLn "--------" >> S.mapM_ print os) .
--   groupByS (\_ b -> getDur b == 0) . f =<< runPIO m

-- dumpMsgTo :: Num a => Int -> Msg a -> IO ()
-- dumpMsgTo n msg = dumpMsgWith (S.take n) msg

-- dumpMsgFromTo :: Num a => Int -> Int -> Msg a -> IO ()
-- dumpMsgFromTo from to msg = dumpMsgWith (S.take to . S.drop from) msg
