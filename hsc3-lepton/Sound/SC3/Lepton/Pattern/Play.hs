{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (FlexibleInstances, BangPatterns)

Sends OSC message sequentially with responding to server.
-}
module Sound.SC3.Lepton.Pattern.Play where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Monad

import Data.Unique
import Sound.OpenSoundControl
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.UGen.Factory

import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.Vector.Fusion.Stream as S

-- ----------------------------------------------------------------------------
-- Types

-- | Composable, audible event pattern.
type Msg a = R (ToOSC a)


-- | Pattern for 's_new' messages.
class Psnew s where
  psnew :: String
    -- ^ Synthdef name.
    -> Maybe Int
    -- ^ Node id for new synth. 'Nothing' for auto generated id by server.
    -> AddAction
    -- ^ Add action in 's_new' message.
    -> Int
    -- ^ Add target id.
    -> [(String, s Double)]
    -- ^ Parameter name and its values.
    -- -> s (ToOSC Double)
    -> s (ToOSC Double)

instance Psnew R where
  psnew = mkSnew

-- instance Psnew S where
--   psnew def nid aa tid ms =
--     S (\_ -> show $ Sn def nid aa tid (M.fromList (map ms))

-- | Pattern for 'n_set' message.
class Pnset s where
  pnset ::
    Int
    -- ^ Target node id.
    -> [(String, s Double)]
    -- ^ Parameter name and its values.
    -> s (ToOSC Double)

instance Pnset R where
  pnset = mkNset

-- instance Pnset S where
--   pnset i ms = S (\_ -> show $ ToOSC (Nset i) (M.fromList ms))

instance Audible (R (ToOSC Double)) where
  play fd r = bracket
    (do send fd (notify True)
        trid <- newNid
        return (fd,trid))
    (\(fd',trid) ->
      send fd' (bundle immediately [notify False, n_free [trid]]))
    (\(fd',trid) -> runMsg r trid fd')

-- ---------------------------------------------------------------------------
-- Guts

-- | Run message.
--
-- When 'NaN' is found in 'freq' key of Map, replace the message
-- with sending 's_new silence'.
--
runMsg :: Transport t
   => Msg Double
   -- ^ Pattern to play
   -> Int
   -- ^ Node id used for trigger synth
   -> t
   -- ^ Destination
   -> IO ()
runMsg msg trid fd = bracket_ newTrigger freeTrigger work where
  --
  -- When delta time is small amount, latency (> 1ms) occurs in
  -- server side. Sending messages in (offsetDelay * 2) chunk, accumulate
  -- until enough duration has been passed in between messages.
  --
  -- Try pattern converter and sender running in different thread, with
  -- passing around chunks via MVar. This may save couple computation time.
  --
  newTrigger = do
    send fd $ s_new "tr" trid AddToHead 1 []
  freeTrigger = send fd $ n_free [trid]
  work = do
    now <- utcr
    foldPIO_ go (now,now,[]) msg
  go (t0,t1,acc) o
    | getDur o == 0 || null acc = return (t0,t1,o:acc)
    | otherwise                 = do
      let dt = getDur o
          enoughTime = (t0+dt)-t1 > (offsetDelay*2)
      msgs <- mkOSCs acc trid
      send fd $ bundle (UTCr $ t0+offsetDelay) (tick:msgs)
      t1' <- if enoughTime then utcr else return t1
      when enoughTime $ waitUntil fd "/tr" trid
      return (t0+dt,t1',[o])
  tick = n_set trid [("t_trig",1)]

{-# SPECIALISE runMsg :: Msg Double -> Int -> UDP -> IO () #-}
{-# SPECIALISE runMsg :: Msg Double -> Int -> TCP -> IO () #-}

mkOSCs :: [ToOSC Double] -> Int -> IO [OSC]
mkOSCs os tid = foldM f v os where
  f acc o
    | isSilence o = do
      nid <- newNid
      let slt = Snew "rest" (Just nid) AddToTail 1
          rest = asOSC (ToOSC slt (M.singleton "dur" (cueDur o)))
      slt `seq` rest `seq` return (rest:acc)
    | otherwise   = case oscType o of
      Snew _ nid _ _ -> do
        nid' <- maybe newNid return nid
        let ops = M.foldrWithKey (mkOpts nid') [] (oscMap o)
            o' = asOSC (setNid nid' o)
        o' `seq` ops `seq` return ((toOSC (setNid nid' o):ops) ++ acc)
      Nset nid -> do
        let t = ToOSC (Nset tid) (M.singleton "t_trig" 1 :: M.Map String Double)
            o' = asOSC o
        o' `seq` t `seq` return (o':asOSC t:acc)
  v = []
  isSilence m = isRest m
  -- isSilence m = (isNaN <$> M.lookup "freq" (oscMap m)) == Just True

-- | Wraps sending @notify True@ and @notify False@ messages before and
-- after 'withSC3'.
sc3 :: (UDP -> IO a) -> IO a
sc3 k = withSC3 $ \fd -> bracket
  (async fd (notify True) >> return fd)
  (\fd' -> send fd' $ notify False)
  k

-- | Synthdef used for responding to 'n_set' messages.
--
-- Contains 'sendTrig' UGen.
tr :: UGen
tr = sendTrig ("t_trig"@@0) ("id"@@0) ("val"@@0)

-- | Silent UGen with done action.
silenceS :: UGen
silenceS = freeSelf (impulse KR 1 0)

-- | Sends synthdefs used by pattern player.
setup :: Transport t => t -> IO OSC
setup fd = async fd $ bundle immediately
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
  Transport t
  => t
  -> String
  -- ^ String to match in returned OSC message.
  -> Int
  -- ^ Int to match in first element of returned OSC message.
  -> IO ()
waitUntil fd str n = recv fd >>= \m -> case m of
  Message !str' (Int !n':_) | str == str' && n == n' -> return ()
  _                         -> waitUntil fd str n
{-# SPECIALISE waitUntil :: UDP -> String -> Int -> IO () #-}
{-# SPECIALISE waitUntil :: TCP -> String -> Int -> IO () #-}

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

-- | 'NaN' value made by @0/0@.
nan :: Floating a => a
nan = 0/0
{-# SPECIALIZE nan :: Double #-}

-- | Make 's_new' messages.
-- mkSnew :: Num a => AddAction -> Int -> String -> [(String, R a)] -> Msg a
-- mkSnew ::
--   Floating a =>
--   String -> Maybe Int -> AddAction -> Int -> [(String, R a)] -> Msg a
-- mkSnew def nid aa tid ms = ToOSC o <$> ms' where
--   o = Snew def nid aa tid
--   ms' = R $ \g ->
--     tail $ shiftT 0 $
--     unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g
mkSnew def nid aa tid ms = ToOSC sn <$> ms' where
  sn = Snew def nid aa tid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g

-- {-# SPECIALISE mkSnew ::
--     String -> Maybe Int -> AddAction -> Int
--     -> [(String, R Double)] -> Msg Double #-}
--
-- | Make 'n_set' messages.
mkNset :: Floating a => NodeId -> [(String, R a)] -> Msg a
mkNset nid ms = ToOSC o <$> ms' where
  o = Nset nid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g
{-# SPECIALISE mkNset :: Int -> [(String,R Double)] -> Msg Double #-}

initialT :: Num a => M.Map String a
initialT = M.singleton "dur" 0
{-# SPECIALIZE initialT :: M.Map String Double #-}

shiftT :: Floating a => a -> [M.Map String a] -> [M.Map String a]
shiftT t ms = case ms of
  (m1:m2:r) ->
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  (m1'):shiftT t' (m2:r)
  [m1] ->
    -- XXX:
    -- Sending dummy silent event at the end of list
    -- to receive /n_go response from server.
    --
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  [m1',M.fromList [("freq",nan),("dur",t')]]
  _ -> []

-- shiftT ::
--   Floating a => a -> S.Stream (M.Map String a) -> S.Stream (M.Map String a)
-- shiftT t ms
--   | S.null ms          = S.empty
--   | S.null (S.tail ms) =
--     let m1  = S.head ms
--         m1' = M.adjust (const t) "dur" m1
--         t'  = M.findWithDefault 0 "dur" m1
--         end = S.singleton $ M.fromList [("freq",nan),("dur",t')]
--     in  S.cons m1' end
--   | otherwise          =
--     let m1  = S.head ms
--         m1' = M.adjust (const t) "dur" m1
--         m2  = S.head (S.tail ms)
--         r   = S.drop 2 ms
--         t'  = M.findWithDefault 0 "dur" m1
--     in  S.cons m1' (shiftT t' (S.cons m2 r))

-- ----------------------------------------------------------------------------
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
