------------------------------------------------------------------------------
-- | Scratch for querying scsynth tree.
--

module SCQuery where

import Sound.SC3 hiding
    ( free,
      select )
import Sound.OpenSoundControl

import SCTree
import Reusable
import Instances

import Control.Monad
import Control.Monad.Reader
import Data.Generics

type Query a = ReaderT UDP IO a

-- | EXample:
-- > > query allNodes s
-- > > query freeAll s
-- > > query (add 1 $ Synth 1001 "foo" []) s
-- > > query (free 1001) s
query :: Query a -> IO UDP -> IO a
query q fd = runReaderT q =<< fd

-- | Default server on localhost, UDP port 57110.
s :: IO UDP
s = openUDP "127.0.0.1" 57110

test :: Query ()
test = do
  fd <- ask
  lift $ send fd $ Message "/n_trace" [Int 0]

allNodes :: Query SCTree
allNodes = do
  fd <- ask
  lift $ do
    send fd (Message "/g_queryTree" [Int 0,Int 1])
    msg <- wait fd "/g_queryTree.reply"
    return $ parseOSC msg

dump :: Query ()
dump = do
  tree <- allNodes
  lift $ putStr $ drawSCTree tree

load :: String -> UGen -> Query OSC
load name ugen = do
  fd <- ask
  lift $ do
    writeSynthdef name ugen
    send fd $ d_recv (synthdef name ugen)
    wait fd "/done"

add :: NodeId -> SCTree -> Query ()
add nId node = do
  fd <- ask
  lift $ send fd $
       case node of
         (g@(Group gid ts)) ->
             if gid /= 0 then
                 Bundle (NTPi 0) (gNew' (Group nId [g]) [])
             else
                 Bundle (NTPi 0) (gNew' g [])
         (s@(Synth nid' name ps)) ->
             s_new name nid' AddToTail nId (concatMap paramToTuple ps)

nfree :: NodeId -> Query ()
nfree nId = do
  fd <- ask
  lift $ send fd $ Message "/n_free" [Int nId]

freeAll :: Query ()
freeAll = nfree 1 >> add 0 (Group 1 [])

type NodeInfo a  = SCTree -> a
type Condition = NodeInfo Bool


-- | Ex.
-- > > query (select $ name ==? "fm1") s
-- > > query (select $ nodeId ==? 1001) s
select :: Condition -> Query [SCTree]
select p = do
  tree <- allNodes
  return $ everything (++) ([] `mkQ` f) tree
    where
      f s | p s = [s]
          | otherwise = []

nodeId :: NodeInfo Int
nodeId (Group i _) = i
nodeId (Synth i _ _) = i

name :: NodeInfo String
name (Group _ _) = ""
name (Synth _ name _) = name

params :: NodeInfo [SynthParam]
params (Group _ _) = []
params (Synth _ _ ps) = ps

-- | Ex. query (select $ paramNames `has` "freq" &&? name ==? "fm") s
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

(&&?) = liftN2 (&&)
(||?) = liftN2 (||)

infixr 3 &&?
infixr 2 ||?

has :: Eq a => NodeInfo [a] -> a -> NodeInfo Bool
has = liftN (flip elem)

liftParam :: ([SynthParam] -> Bool) -> NodeInfo Bool
liftParam f (Synth _ _ ps) = f ps
liftParam f _ = False


-- nodeId :: Int -> Query (Maybe SCTree)
-- nodeId nId = do
--   tree <- allNodes
--   return $ everything orElse (Nothing `mkQ` f) tree
--     where
--       f s | getNodeId s == nId = Just s
--           | otherwise = Nothing
--       getNodeId s@(Synth n _ _) = n
--       getNodeId g@(Group n _) = n

-- name :: SynthName -> Query [SCTree]
-- name name = do
--   tree <- allNodes
--   return $ everything (++) ([] `mkQ` f) tree
--     where
--       f s@(Synth _ name' _) | name == name' = [s]
--                             | otherwise = []
--       f _ = []

param :: ParamName -> Query [SCTree]
param name = do
  tree <- allNodes
  return $ everything (++) ([] `mkQ` f) tree
    where
      f s@(Synth _ _ ps) | name `elem` (map getParamName ps) = [s]
                         | otherwise = []
      f _ = []
      getParamName (n:=_) = n
      getParamName (n:<-_) = n

--
-- Playing with datatype.
--

data NI a = NI {runNI :: SCTree -> a}

instance Show (NI a) where
    show _ =  "<NodeInfo>"

instance Monad NI where
    return a = NI (\_ -> a)
    NI f >>= k =  NI (\t -> (runNI (k (f t)) t))

testNI a = runNI (return (==) `ap` NI name `ap` return a)
