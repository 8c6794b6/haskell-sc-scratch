{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Sequential OSC message sending without Control.Concurrent.threadDelay.

Adding higher level language feature, sequencing multiple messages.

we have a sequence like:

> m1, m2 :: Msg Double

What can we do when we want to play m2 after m1?

We would like to write a function:

> mseq :: Msg a -> Msg a -> Msg a

which concatenate given messages sequentially, and:

> mpar :: Msg a -> Msg a -> Msg a

which runs given messages in parallel, and:

> runMsg :: Transport t => Msg a -> t -> IO ()

to send and receive messages sequentially to specified server.

... And, 'Msg' turned out to be a type synonym for 'R (ToOSC a)'.

-}
module Sound.SC3.Lepton.Respond where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Sequence (Seq)
import System.Random (StdGen)

import Data.List.Stream (groupBy)
import Data.Unique
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton.Pattern
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.UGen.Factory

import qualified Control.Parallel as CP
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import qualified Data.Traversable as T
import qualified Data.Vector.Fusion.Stream as S

-- | Wraps sending @notify True@ and @notify False@ messages before and
-- after 'withSC3'.
sc3 :: (UDP -> IO a) -> IO a
sc3 k = withSC3 $ \fd -> bracket
  (async fd (notify True) >> return fd)
  (\fd' -> send fd' $ notify False)
  k

w :: (UDP -> IO a) -> IO a
w = sc3

-- | Synthdef used for responding to 'n_set' messages.
--
-- Contains 'sendTrig' UGen.
tr :: UGen
tr = sendTrig ("t_trig"@@0) ("id"@@0) ("val"@@0)

-- | Silent UGen with done action.
silenceS :: UGen
silenceS = freeSelf (impulse KR 1 0)

-- | Synthdef to
doneS :: UGen
doneS = freeSelf (impulse KR 1 0)

setup :: Transport t => t -> IO OSC
setup fd = async fd $ bundle immediately
  [d_recv $ synthdef "tr" tr
  ,d_recv $ synthdef "silence" silenceS
  ,d_recv $ synthdef "rest" silenceS
  ,d_recv $ synthdef "done" doneS]

offsetDelay :: Double
offsetDelay = 0.1

newNid :: IO Int
newNid = (+ 10000) . hashUnique <$> newUnique

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

-- ----------------------------------------------------------------------------
-- Types

-- | Composable and audible events pattern.
type Msg a = R (ToOSC a)

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

-- ---------------------------------------------------------------------------
-- Classes and instances

-- | Merge two patterns.
class Pmerge p where
  pmerge :: Mergable (p m) => p m -> p m -> p m

-- | Parallelise patterns in given list.
class Ppar p where
  ppar :: Mergable (p m) => [p m] -> p m

instance Pmerge R where
  pmerge = merge

instance Ppar R where
  ppar = foldr1 pmerge

instance Pmerge S where
  pmerge a b = S (\_ -> "pmerge (" ++ showP a ++ ") (" ++ showP b ++ ")")

instance Ppar S where
  ppar ps = S (\_ -> "ppar " ++ showList ps "")

class Mergable m where
  merge :: m -> m -> m

instance Show a => Mergable (S a) where
  merge a b = S (\_ -> unwords ["merge", show a, show b])

instance (Ord a, Num a) => Mergable (R (ToOSC a)) where
  merge p1 p2 = R $ \g ->
    let p1' = unR p1 g
        p2' = unR p2 g
    in  p1' `CP.par` (p2' `CP.pseq` merge p1' p2')

instance (Ord a, Num a) => Mergable [ToOSC a] where
  merge a b = mergeL 0 (0,0) a b

instance (Ord a, Num a) => Mergable (S.Stream (ToOSC a)) where
  merge a b = mergeS 0 (0,0) a b

mergeL ::
  (Ord a, Num a) =>
  a -> (a, a) -> [ToOSC a] -> [ToOSC a] -> [ToOSC a]
mergeL _ _ [] [] = []
mergeL t (ta,_) (a:as) [] = tadjust "dur" (const $ getDur a + ta - t) a : as
mergeL t (_,tb) [] (b:bs) = tadjust "dur" (const $ getDur b + tb - t) b : bs
mergeL t (ta,tb) (a:as) (b:bs)
  | ua <= ub  = tadjust "dur" (const $ ua-t) a : mergeL ua (ua,tb) as (b:bs)
  | otherwise = tadjust "dur" (const $ ub-t) b : mergeL ub (ta,ub) (a:as) bs
  where
    ua = ta + getDur a
    ub = tb + getDur b

mergeS ::
  (Ord a, Num a) =>
  a -> (a, a) -> S.Stream (ToOSC a) -> S.Stream (ToOSC a) -> S.Stream (ToOSC a)
mergeS t (ta,tb) ass bss
  | S.null ass && S.null bss = S.empty
  | S.null ass =
    let a = S.head as
    in  tadjust "dur" (const $ ua-t) a `S.cons` S.tail as
  | S.null bss =
    let b = S.head bs
    in  tadjust "dur" (const $ ub-t) b `S.cons` S.tail bs
  | ua <= ub  = tadjust "dur" (const $ ua - t) a `S.cons` mergeS ua (ua,tb) as bss
  | otherwise = tadjust "dur" (const $ ub - t) b `S.cons` mergeS ub (ta,ub) bs ass
  where
    as = S.tail ass
    bs = S.tail bss
    a = S.head ass
    b = S.head ass
    ua = ta + getDur a
    ub = tb + getDur b

class Event e where
  type Evalue e :: *
  eOSC :: e (Evalue e) -> OSC
  eDur :: e (Evalue e) -> Double

instance Event ToOSC where
  type Evalue ToOSC = Double
  eOSC = toOSC
  eDur = getDur

class Snew s where
  snew :: String -> Maybe Int -> AddAction -> Int
       -> [(String, s Double)] -> s (ToOSC Double)

instance Snew R where
  snew = mkSnew

instance Snew S where
  snew def nid aa tid ms =
    S (\_ -> show $ ToOSC (Snew def nid aa tid) (M.fromList ms))

class Nset s where
  nset :: Int -> [(String, s Double)] -> s (ToOSC Double)

instance Nset R where
  nset = mkNset

instance Nset S where
  nset i ms = S (\_ -> show $ ToOSC (Nset i) (M.fromList ms))


-- | Run message.
--
-- When 'NaN' is found in 'freq' key of Map, replace the message
-- with sending 's_new silence'.
--
-- When delta time is small amount (> 10ms), latency occurs in
-- server side. Modify to send messages in chunk, accumulate until
-- enough duration has been passed in between messages. Try out from
-- sending once in every more than 20 ms.
--
-- Try pattern converter and sender running in different thread. This
-- may save couple computation time.
--
runMsg :: Transport t => Msg Double -> t -> IO ()
runMsg msg fd = do
  now  <- utcr
  trid <- newNid
  send fd $ s_new "tr" trid AddToHead 1 []
  oscs <- runPIO msg
  foldM_ (f trid) (now,now) . groupBy (\_ b -> getDur b == 0) $ oscs
  where
    f _    times   []       = return times
    f trid (t0,tl) os@(o:_) = do
      let dt   = getDur o
          wait = (t0+dt)-tl > (offsetDelay*10)
      (!nid:_,!msgs@(msg:_)) <- mkOSCs os trid
      send fd $ bundle (UTCr $ t0+dt+offsetDelay) msgs
      tl' <- if wait then utcr else return tl
      when wait $ case msg of
        Message "/s_new" _ -> waitUntil fd "/n_go" nid
        Message "/n_set" _ -> waitUntil fd "/tr" trid
        _                  -> return ()
      return (t0+dt,tl')

-- runMsg :: Transport t => Msg Double -> t -> IO ()
-- runMsg msg fd = do
--   now  <- utcr
--   trid <- newNid
--   send fd $ s_new "tr" trid AddToHead 1 []
--   oscs <- runPIO msg
--   S.foldM (f trid) (now,now) . groupByS (\_ b -> getDur b == 0) $ oscs
--   return ()
--   where
--     f trid (t0,tl) os
--       | S.null os = return (t0,tl)
--       | otherwise = do
--         let o = S.head os
--             dt = getDur o
--             wait = t0-tl > offsetDelay -- (offsetDelay*5)
--         (!nid:_,!msgs@(msg:_)) <- mkOSCs (S.toList os) trid
--         send fd $ bundle (UTCr $ t0+dt+offsetDelay) msgs
--         tl' <- if wait then utcr else return tl
--         when wait $ case msg of
--           Message "/s_new" _ -> waitUntil fd "/n_go" nid
--           Message "/n_set" _ -> waitUntil fd "/tr" trid
--           _                  -> return ()
--         return (t0+dt,tl')

-- {-# SPECIALISE runMsg :: Msg Double -> UDP -> IO () #-}
-- {-# SPECIALISE runMsg :: Msg Double -> TCP -> IO () #-}
--
-- | 'NaN' value made by @@0/0@@.
nan :: Floating a => a
nan = 0/0
{-# SPECIALIZE nan :: Double #-}

mkOSCs :: [ToOSC Double] -> Int -> IO ([NodeId],[OSC])
mkOSCs os tid = foldM f v os where
  f (ns,acc) o
    | isSilence o = do
      nid <- newNid
      let slt = Snew "rest" (Just nid) AddToTail 1
          rest = toOSC (ToOSC slt (M.singleton "dur" (getDur o)))
      slt `seq` rest `seq` return (nid:ns, rest:acc)
    | otherwise   = case oscType o of
      Snew _ nid _ _ -> do
        nid' <- maybe newNid return nid
        let ops = M.foldrWithKey (mkOpts nid') [] (oscMap o)
            o' = toOSC (setNid nid' o)
        o' `seq` ops `seq` return (nid':ns, (toOSC (setNid nid' o):ops) ++ acc)
      Nset nid -> do
        let t = ToOSC (Nset tid) (M.singleton "t_trig" 1)
            o' = toOSC o
        o' `seq` t `seq` return (nid:ns, o':toOSC t:acc)
  v = ([],[])
  isSilence m = (isNaN <$> M.lookup "freq" (oscMap m)) == Just True

-- | Make 's_new' messages.
-- mkSnew :: Num a => AddAction -> Int -> String -> [(String, R a)] -> Msg a
mkSnew ::
  Floating a =>
  String -> Maybe Int -> AddAction -> Int -> [(String, R a)] -> Msg a
mkSnew def nid aa tid ms = ToOSC o <$> ms' where
  o = Snew def nid aa tid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g

  -- ms' = R $ \g ->
  --   S.tail $ shiftT 0 $
  --   S.cons initialT (unR (T.sequenceA $ M.fromList ms) g)

{-# SPECIALISE mkSnew ::
    String -> Maybe Int -> AddAction -> Int
    -> [(String, R Double)] -> Msg Double #-}

-- | Make 'n_set' messages.
mkNset :: Floating a => NodeId -> [(String, R a)] -> Msg a
mkNset nid ms = ToOSC o <$> ms' where
  o = Nset nid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    initialT : unR (T.sequenceA $ M.fromList ms) g
{-# SPECIALISE mkNset :: Int -> [(String,R Double)] -> Msg Double #-}

-- mkNset nid ms = ToOSC o <$> ms' where
--   o = Nset nid
--   ms' = R $ \g ->
--     S.tail $ shiftT 0 $
--     S.cons initialT (unR (T.sequenceA $ M.fromList ms) g)

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
--     let m1 = S.head ms
--         m1' = M.adjust (const t) "dur" m1
--         t' = M.findWithDefault 0 "dur" m1
--         end = S.singleton $ M.fromList [("freq",nan),("dur",t')]
--     in  S.cons m1' end
--   | otherwise          =
--     let m1 = S.head ms
--         m1' = M.adjust (const t) "dur" m1
--         m2 = S.head (S.tail ms)
--         r  = S.drop 2 ms
--         t' = M.findWithDefault 0 "dur" m1
--     in  S.cons m1' (shiftT t' (S.cons m2 r))

toOSC :: ToOSC Double -> OSC
toOSC (ToOSC t m) = case t of
  Snew def nid aa tid -> s_new def (fromMaybe (-1) nid) aa tid (M.assocs m)
  Nset nid            -> n_set nid (M.assocs m)

setNid :: NodeId -> ToOSC a -> ToOSC a
setNid nid o = case oscType o of
  Snew def _ aa tid -> ToOSC (Snew def (Just nid) aa tid) (oscMap o)
  Nset _            -> ToOSC (Nset nid) (oscMap o)

getDur :: Num a => ToOSC a -> a
getDur o = fromMaybe 1 $ M.lookup "dur" (oscMap o)
{-# SPECIALISE getDur :: ToOSC Double -> Double #-}

tadjust :: String -> (a -> a) -> ToOSC a -> ToOSC a
tadjust k f (ToOSC ot om) = ToOSC ot (M.adjust f k om)

madjust :: Functor f => String -> (a -> a) -> f (ToOSC a) -> f (ToOSC a)
madjust k f r = fmap (tadjust k f) r
{-# SPECIALISE madjust :: String -> (a -> a) -> R (ToOSC a) -> R (ToOSC a) #-}

-- ----------------------------------------------------------------------------
-- Stream functions

-- groupByS :: (a -> a -> Bool) -> S.Stream a -> S.Stream (S.Stream a)
-- groupByS p xs
--   | S.null xs = S.empty
--   | otherwise = (x `S.cons` ys) `S.cons` groupByS p yss
--   where
--     x = S.head xs
--     (ys,yss) = spanS (p x) (S.tail xs)

-- spanS :: (a -> Bool) -> S.Stream a -> (S.Stream a, S.Stream a)
-- spanS p ss = (S.takeWhile p ss,S.dropWhile p ss)

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

-- ---------------------------------------------------------------------------
-- Sample

silence n = snew "silence" Nothing AddToTail 1 [("dur",n)]

m1 = snew "rspdef1" Nothing AddToTail 1
  [("dur", plist [1/4,3/4])
  ,("freq",fmap midiCPS $ pcycle [67,69])
  ,("pan", pforever 0.3)]

m2 = snew "rspdef1" Nothing AddToTail 1
  [("dur",  pseq 2 [3/4,1/4,2/4,2/4])
  ,("freq", fmap midiCPS $ pseq 3 [60,62,63,65,67,68,70,72])]

m3 = snew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 2 [2/8,2/8,3/8,1/8])
  ,("freq", fmap midiCPS $ pcycle $ [60,67,74,81])]

m4 = snew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 32 [1/32])
  ,("freq", fmap midiCPS $ pcycle [115,103])]

m5 = snew "rspdef1" Nothing AddToTail 1
  [("dur", pseq 8 [1/4])
  ,("pan", prepeat 0.75)
  ,("freq", fmap midiCPS $ pcycle [60,64,67,72])]

m6 = snew "rspdef1" Nothing AddToTail 1
  [("dur", plist [7/8, 1/8, 6/13, 7/13])
  ,("pan", prepeat (-0.75))
  ,("freq", fmap midiCPS $ plist [84,86,89,91])]

m7 = silence 2

m8 =
  ppar
  [pforever
   (pchoose 1 [m2,m3,m4,m5,m6,m7])
  ,pcycle
   [madjust "freq" (*2) m1
   ,madjust "freq" (*1.5) m1]
  ,pforever
   (madjust "freq" (*0.5) $
    madjust "pan" (const (-0.8)) $
    madjust "dur" (*2) $
    madjust "atk" (const 1) $
    m1)]
