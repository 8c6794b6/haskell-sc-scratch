{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Querying scsynth tree.
--
-- /Examples/
--
-- > > query s allNodes
-- > > query s freeAll
-- > > query s (add 1 $ Synth 1001 "foo" [])
-- > > query s (nfree 1001)
-- > > query s (sel $ name ==? "fm1")
-- > > query s (sel $ nodeId ==? 1001)
-- > > query s (sel $ paramNames `has` "freq" &&? name ==? "fm")
--
module Sound.SC3.Lepton.Query where

import Control.Exception (bracket)
import "mtl" Control.Monad.Reader
  ( ReaderT(..)
  , MonadReader(..)
  , MonadIO(..) )

import Data.Generics.Uniplate.Data (universe)
import Sound.SC3 hiding (free)
import Sound.OpenSoundControl

import Sound.SC3.Lepton.Tree
import Sound.SC3.Lepton.Util
import Sound.SC3.Lepton.Instance ()

-- | Query for udp connection.
newtype Query a = Query {runQuery :: ReaderT UDP IO a}
                deriving (Functor, Monad, MonadIO, MonadReader UDP)

-- | Send query to given connection.
query :: IO UDP -> Query a -> IO a
query fd q = bracket fd close (runReaderT (runQuery q))

-- | Default server on localhost, UDP port 57110.
s :: IO UDP
s = openUDP "127.0.0.1" 57110

-- | Latency.
latency :: Double
latency = 0.01

bundle :: Double -> [OSC] -> OSC
bundle time ms = Bundle (UTCr (time + latency)) ms

allNodes :: Query SCTree
allNodes = do
  fd <- ask
  liftIO $ do
    send fd (Message "/g_queryTree" [Int 0,Int 1])
    m <- wait fd "/g_queryTree.reply"
    return $ parseOSC m

dump :: Query ()
dump = do
  fd <- ask
  liftIO $ printRootNode fd

load :: String -> UGen -> Query OSC
load n ugen = do
  fd <- ask
  liftIO $ do
    writeSynthdef n ugen
    async fd $ d_recv (synthdef n ugen)

add :: NodeId -> SCTree -> Query ()
add nId node = do
  fd <- ask
  liftIO $ do
    now <- utcr
    send fd $
       case node of
         g@(Group gid _) ->
             if gid /= 0 then
                 bundle now $ treeToOSC $ Group nId [g]
             else
                 bundle now $ treeToOSC g
         Synth nid' n ps ->
             bundle now $
               [s_new n nid' AddToTail nId (concatMap paramToTuple ps)]

nfree :: NodeId -> Query ()
nfree nId = msg $ Message "/n_free" [Int nId]

addDefault :: Query ()
addDefault = msg $ g_new [(1,AddToTail,0)]

freeAll :: Query ()
freeAll = msg $ g_freeAll [1]

msg :: OSC -> Query ()
msg oscMsg = do
  fd <- ask
  liftIO $ do
    now <- utcr
    send fd $ bundle now [oscMsg]

type NodeInfo a  = SCTree -> a
type Condition = NodeInfo Bool


-- | Select with given condition.
sel :: Condition -> Query [SCTree]
sel p = do
  tree <- allNodes
  return $ [n | n <- universe tree, p n]

nodeId :: NodeInfo Int
nodeId (Group i _) = i
nodeId (Synth i _ _) = i

name :: NodeInfo String
name (Group _ _) = ""
name (Synth _ n _) = n

params :: NodeInfo [SynthParam]
params (Group _ _) = []
params (Synth _ _ ps) = ps

-- | Condition for param names.
paramNames :: NodeInfo [ParamName]
paramNames (Synth _ _ ps) = map getName ps
    where getName (n:=_) = n
          getName (n:<-_) = n
paramNames _ = []

liftN ::  (a -> b -> c) -> NodeInfo a -> b -> NodeInfo c
liftN q f v = \n -> f n `q` v

liftN2 :: (a -> b -> c)  -> NodeInfo a -> NodeInfo b -> NodeInfo c
liftN2 q f g = \n -> f n `q` g n

(==?),(/=?) :: Eq a => NodeInfo a -> a -> NodeInfo Bool
(==?) = liftN (==)
(/=?) = liftN (/=)

infix 4 ==?

(>?),(>=?),(<?),(<=?) :: Ord a => NodeInfo a -> a -> NodeInfo Bool
(>?) = liftN (>)
(>=?) = liftN (>=)
(<?) = liftN (<)
(<=?) = liftN (<=)

infix 4 >?
infix 4 >=?
infix 4 <?
infix 4 <=?

(&&?), (||?) :: NodeInfo Bool -> NodeInfo Bool -> NodeInfo Bool
(&&?) = liftN2 (&&)
(||?) = liftN2 (||)

infixr 3 &&?
infixr 2 ||?

has :: Eq a => NodeInfo [a] -> a -> NodeInfo Bool
has = liftN (flip elem)

liftParam :: ([SynthParam] -> Bool) -> NodeInfo Bool
liftParam f (Synth _ _ ps) = f ps
liftParam _ _ = False

param :: ParamName -> Query [SCTree]
param n = do
  tree <- allNodes
  return $ [x | x@(Synth _ _ ps) <- universe tree, n `elem` map f ps]
  where
    f (x:=_)  = x
    f (x:<-_) = x
