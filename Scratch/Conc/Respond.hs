{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Sequential OSC message sending without Control.Concurrent.threadDelay.

-}
module Respond where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Monoid
import System.Random (StdGen)

import Data.List.Stream (groupBy)
import Data.Unique
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import qualified Control.Parallel as CP
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

-- | Synthdef to
doneS :: UGen
doneS = freeSelf (impulse KR 1 0)

setup :: Transport t => t -> IO OSC
setup fd = async fd $ bundle immediately
  [d_recv $ synthdef "tr" tr
  ,d_recv $ synthdef "done" doneS]

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
sNew aa tid def ps fd = join $ foldM_ f <$> utcr <*> k ps where
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


{-

So far, above message sequence is working well.

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

-}

newtype Msg a = Msg { unMsg :: R (ToOSC a) }
  deriving (Eq, Show)

instance Functor Msg where
  fmap f (Msg r) = Msg (fmap (fmap f) r)

instance Monoid (Msg a) where
  mappend = mseq
  mempty  = Msg pempty

data ToOSC a = ToOSC
  { oscType :: MsgType
  , oscMap  :: M.Map String a
  , oscLast :: Bool
  } deriving (Eq, Show)

instance Functor ToOSC where
  fmap f (ToOSC t m l) = ToOSC t (fmap f m) l

data MsgType
  = Snew String (Maybe NodeId) AddAction NodeId
  | Nset NodeId
    deriving (Eq, Show)

-- | Run message.
runMsg :: Transport t => Msg Double -> t -> IO ()
runMsg msg fd = do
  now <- utcr
  foldM_ f now . groupBy (\_ b -> getDur b == 0) {- . shiftT' 0 -} =<<
    runPIO (unMsg msg)
  where
    f :: Double -> [ToOSC Double] -> IO Double
    f t0 os
      | null os = return t0
      | otherwise = do
        let o  = head os
            dt = getDur o
        (nid,msgs) <- mkOSCs os
        -- print dt
        -- mapM_ print msgs
        send fd $ bundle (UTCr $ t0+dt+offsetDelay) msgs
        waitUntil fd "/n_go" nid
        return (t0+dt)
      -- | otherwise = do
      --   let o  = head os
      --       dt = getDur o
      --   nid <- newNid
      --   send fd $ bundle (UTCr $ t0+dt+offsetDelay)
      --     [s_new "tr" nid AddToTail 0 []]
      --   waitUntil fd "/n_go" nid
      --   send fd $ n_free [nid]
      --   return (t0+dt)

{-# SPECIALISE runMsg :: Msg Double -> UDP -> IO () #-}
{-# SPECIALISE runMsg :: Msg Double -> TCP -> IO () #-}

mkOSCs :: [ToOSC Double] -> IO (NodeId,[OSC])
mkOSCs = foldM f v where
  -- XXX: Only supporting s_new messages.
  f (_,acc) o = newNid >>= \nid -> return (nid, toOSC (setNid nid o):acc)
  v = (-1,[])

sendR :: Transport t => R (ToOSC Double) -> t -> IO ()
sendR r fd = runMsg (Msg r) fd

-- | Simply appending contents of 'Msg' with 'pappend'.
mseq :: Msg a -> Msg a -> Msg a
mseq (Msg m1) (Msg m2) = Msg (pappend m1 m2)

-- XXX:
-- Not working as expected. Merge the durations of m1 and m2.
mpar :: (Ord a, Num a) => Msg a -> Msg a -> Msg a
mpar (Msg m1) (Msg m2) = Msg m3 where
  m3 = R $ \g ->
    let p1 = runP m1 g
        p2 = runP m2 g
    in  merge (0,0) p1 p2
  merge _       [] [] = []
  merge (ta,tb) as [] = as
  merge (ta,tb) [] bs = bs
  merge (ta,tb) (a:as) (b:bs) =
    let dta = getDur a
        dtb = getDur b
    in if (ta+dta) <= (tb+dtb) then
         let dt = min dta (tb+dtb - (ta+dta))
         in  tadjust "dur" (const dt) a : merge (ta+dta,tb) as (b:bs)
       else
         let dt = min dtb (ta+dta - (tb+dtb))
         in  tadjust "dur" (const dt) b : merge (ta,tb+dtb) (a:as) bs

getAbsoluteTime :: Num a => ToOSC a -> a
getAbsoluteTime = M.findWithDefault 0 "when" . oscMap

setAbsoluteTime :: Num a => a -> ToOSC a -> ToOSC a
setAbsoluteTime t o = o {oscMap = M.insert "when" t $ oscMap o}

fillAbsoluteTimes :: Num a => [ToOSC a] -> [ToOSC a]
fillAbsoluteTimes = scanl1 $
  \a b -> setAbsoluteTime (getAbsoluteTime a + getDur b) b

-- | Make 's_new' messages.
mkSnew :: Num a => AddAction -> Int -> String -> [(String, R a)] -> Msg a
-- mkSnew aa tid def ms = Msg $ (\m -> ToOSC o m False) <$> ms' where
--   o = Snew def Nothing aa tid
--   ms' = R $ \g -> runP (T.sequenceA $ M.fromList ms) g
mkSnew aa tid def ms = Msg $ (uncurry (flip $ ToOSC o)) <$> ms' where
  o = Snew def Nothing aa tid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    runP (pval initialT `pappend` (T.sequenceA $ M.fromList ms)) g

initialT :: Num a => M.Map String a
initialT = M.singleton "dur" 0

shiftT' :: Num a => a -> [ToOSC a] -> [ToOSC a]
shiftT' t ms = case ms of
  (m1:m2:r) ->
    let m1' = tadjust "dur" (const t)m1
        t'  = getDur m1
    in  m1' : shiftT' t' (m2:r)
  [m1] ->
    let m1' = tadjust "dur" (const t) m1
        t'  = getDur m1
    in  [m1'
        ,ToOSC (Snew "done" Nothing AddToTail 1)
         (M.fromList [("dur",t'),("t_trig",1)]) True]
  _    -> []

shiftT ::
  Num a
  => a
  -> [M.Map String a]
  -> [(Bool,M.Map String a)]
  -- ^ First element is True if last element.
shiftT t ms = case ms of
  (m1:m2:r) ->
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  (False,m1'):shiftT t' (m2:r)
  [m1] ->
    -- XXX: Sending dummy silent event at the end of list.
    -- Using "amp" = 0 message as adhoc solution.
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  [(False,m1'),(True,M.fromList [("dur", t'),("amp",0)])]
  _ -> []

-- | Make 'n_set' messages.
-- mkNset :: Int -> [(String,R a)] -> Msg a
-- mkNset nid ms = ToOSC o <$> ms' where
--   o = Nset nid
--   ms' = R $ runP $ T.sequenceA $ M.fromList ms

toOSC :: ToOSC Double -> OSC
toOSC (ToOSC t m _) = case t of
  Snew def nid aa tid -> s_new def (fromMaybe (-1) nid) aa tid (M.assocs m)
  Nset nid            -> n_set nid (M.assocs m)

setNid :: NodeId -> ToOSC a -> ToOSC a
setNid nid o = case oscType o of
  Snew def _ aa tid -> ToOSC (Snew def (Just nid) aa tid) (oscMap o) (oscLast o)
  Nset _            -> ToOSC (Nset nid) (oscMap o) (oscLast o)

getDur :: (Num a) => ToOSC a -> a
getDur o = fromMaybe 1 $ M.lookup "dur" (oscMap o)

tadjust :: String -> (a -> a) -> ToOSC a -> ToOSC a
tadjust k f (ToOSC ot om ob) = ToOSC ot (M.adjust f k om) ob

madjust :: String -> (a -> a) -> Msg a -> Msg a
madjust k f (Msg r) = Msg $ fmap (tadjust k f) r

-- Sample

m1 = mkSnew AddToTail 1 "rspdef1"
  [("dur", plist [1/4,3/4])
  ,("freq",fmap midiCPS $ pcycle [67,69])]

m2 = mkSnew AddToTail 1 "rspdef1"
  [("dur", pseq 2 [3/4,1/4,2/4,2/4])
  ,("freq", fmap midiCPS $ pseq 3 [60,62,63,65,67,68,70,72])]

m2'seq = madjust "dur" (*0.33)
  (m2 `mseq`
   madjust "freq" (*2) m2 `mseq`
   madjust "freq" (*1.5) m2 `mseq`
   madjust "freq" (*2) m2 `mseq`
   madjust "freq" (*0.75) m2 `mseq`
   m2)

m1m2 = m1 `mpar` m2

m3 = mkSnew AddToTail 1 "rspdef1"
  [("dur", pseq 2 [2/8,2/8,3/8,1/8])
  ,("freq", fmap midiCPS $ pcycle [60,67,74,81])]

m4 = mkSnew AddToTail 1 "rspdef1"
  [("dur", pseq 32 [1/32])
  ,("freq", fmap midiCPS $ pcycle [115,103])]

m5 = mkSnew AddToTail 1 "rspdef1"
  [("dur", pseq 8 [1/4])
  ,("pan", prepeat 0.75)
  ,("freq", fmap midiCPS $ pcycle [60,64,67,72])]

m6 = mkSnew AddToTail 1 "rspdef1"
  [("dur", plist [7/8, 1/8, 6/13, 7/13])
  ,("pan", prepeat (-0.75))
  ,("freq", fmap midiCPS $ plist [84,86,89,91])]
