{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-|

Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Make OSC message with comparing two node tree. The result OSC contains set
of message to modify one node tree to another.

TODO:

* Support moving nodes with n_order .... when a node with same node is was
  included in both of set for insertion and deletion, and the node was
  identical, it's operation will be reordering.

Known bug:

* Updating from t1 to t3, t3 to t6 in Sample.hs is failing. It's freeing the
  parent group after adding new synth nodes.

-}
module SCDiff where

import Control.Monad (when)
import Data.Tree
import Data.List (foldl')
import System.Random (randomRIO)

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import MyDiff
-- import Data.Generic.Diff
import Sample

import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

{-|
Data type defined with GADT for diffing SCNode.

Not so much difference in performance for getting edit distance data, but good
thing is that Synth node and parameters are combined. This makes extraction of
n_set message easier to be done.
-}
data TFamily :: * -> * -> * where
  TFNode     :: TFamily (Tree SCN) (Cons SCN (Cons [Tree SCN] Nil))
  TFNodeNil  :: TFamily [Tree SCN] Nil
  TFNodeCons :: TFamily [Tree SCN] (Cons (Tree SCN) (Cons [Tree SCN] Nil))
  TFN        :: SCN -> TFamily SCN Nil

instance Show (TFamily a b) where
  show TFNode     = "Node"
  show TFNodeNil  = "[]"
  show TFNodeCons = ":"
  show (TFN n)    = show n

instance Family TFamily where
  decEq TFNode TFNode               = Just (Refl,Refl)
  decEq TFNodeNil TFNodeNil         = Just (Refl,Refl)
  decEq TFNodeCons TFNodeCons       = Just (Refl,Refl)
  decEq (TFN a) (TFN b) | a == b    = Just (Refl,Refl)
                        | otherwise = Nothing
  decEq _ _ = Nothing

  fields TFNode (Node x ts) = Just (CCons x (CCons ts CNil))
  fields TFNodeNil []       = Just CNil
  fields TFNodeCons (x:xs)  = Just (CCons x (CCons xs CNil))
  fields (TFN _) _          = Just CNil
  fields _ _                = Nothing

  apply TFNode (CCons x (CCons ts CNil))     = Node x ts
  apply TFNodeNil CNil                       = []
  apply TFNodeCons (CCons x (CCons xs CNil)) = x:xs
  apply (TFN n) CNil                         = n

  string = show

instance Type TFamily (Tree SCN) where
  constructors = [Concr TFNode]

instance Type TFamily [Tree SCN] where
  constructors = [Concr TFNodeNil, Concr TFNodeCons]

instance Type TFamily SCN where
  constructors = [Abstr TFN]

type SCNDiff = EditScript TFamily (Tree SCN) (Tree SCN)

-- | Represents scsynth node in tree, including parameters.
data SCN = Gnode Int
         | Snode Int String [SynthParam]
         deriving (Eq,Show)

-- | Hold where to insert if new node were added.
data Position = Head Int | After Int
              deriving (Eq,Show)

updateNodeFrom :: Transport t => Int -> SCNode -> t -> IO ()
updateNodeFrom i t1 fd = do
  t0 <- getNode i fd
  let msgs = diffMessage t0 t1
  now <- utcr
  send fd $ Bundle (UTCr now) msgs

updateNode :: Transport t => SCNode -> t -> IO ()
updateNode = updateNodeFrom 0

getDiff :: Transport t => Int -> SCNode -> t -> IO SCNDiff
getDiff i t1 fd = do
  t0 <- getNode i fd
  return $ diffSCN t0 t1

diffSCN :: SCNode -> SCNode -> SCNDiff
diffSCN ta tb = diff (toRose ta) (toRose tb)

toRose :: SCNode -> Tree SCN
toRose (Group i ns) = Node (Gnode i) (map toRose ns)
toRose (Synth i n ps) = Node (Snode i n ps) []

dumpDiff :: EditScriptL TFamily txs tys -> IO ()
dumpDiff d = case d of
  Ins n d -> putStrLn ("Ins " ++ show n) >> dumpDiff d
  Cpy n d -> putStrLn ("Cpy " ++ show n) >> dumpDiff d
  Del n d -> putStrLn ("Del " ++ show n) >> dumpDiff d
  _ -> return ()

diffMessage :: SCNode -> SCNode -> [OSC]
diffMessage t = accToOSC . toAcc (initialAcc (nodeId t)) . diffSCN t

-- | OSC Message accumulator
data MsgAcc = MsgAcc
  { -- | Position to insert new node.
    pos :: Position
  , -- | Current group id.
    cgi :: [Int]
  , -- |Map of key=nodeId, val=node, for insert operation.
    inss :: [(Int,(Position,SCNode))]
  , -- | Map of key=nodeId, val=node, for delete operation.
    dels :: IM.IntMap SCNode
  } deriving (Eq,Show)

-- | Initial accumulator, start adding from head of given node id.
initialAcc :: Int -> MsgAcc
initialAcc i = MsgAcc (Head i) [i] [] IM.empty

{-|
Converts edit script to list of operation. What we want to do here is to
convert EditScript to set of operation so that easily converted to OSC
messages.

* When deletion and insertion of node with same id occur, operation
  is updating the node parameter or moving the node.

* When deletion of node comes without insertion of node with same id, delete
  the node.

* When insertion of node comes before other than deletion of node,
  operation is adding new node. We detect this with seeking an
  editscript whether Ins of same node appear or not. Also, need to keep track
  of target node id, and position. Note that AddAfter add action could not be
  used when adding to group without containing any node yet.

This function seeks 2 sequence of EditScriptL constructors at once, and
determine which operation to take.
-}
toAcc :: forall txs tys . MsgAcc -> EditScriptL TFamily txs tys -> MsgAcc
toAcc acc d0 = case d0 of
  Ins TFNodeNil (Ins TFNodeNil d) -> toAcc upOneGroup d
  Ins TFNodeNil (Cpy TFNodeNil d) -> toAcc upOneGroup d
  Ins (TFN (Snode i n ps)) d ->
    let inss' = (i,(pos acc, Synth i n ps)):inss acc
    in  toAcc (acc{inss=inss', pos=After i}) d
  Ins (TFN (Gnode i)) d ->
    let inss' = (i,(pos acc, Group i [])):inss acc
    in  toAcc (acc{pos=Head i, cgi=i:cgi acc,inss=inss'}) d
  Ins _ d -> toAcc acc d
  Del (TFN n) d -> toAcc (acc {dels=IM.insert (scnId n) (scn2n n) (dels acc)}) d
  Del _ d -> toAcc acc d
  Cpy TFNodeNil (Cpy TFNodeNil d) -> toAcc upOneGroup d
  Cpy TFNodeNil (Ins TFNodeNil d) -> toAcc upOneGroup d
  Cpy (TFN (Snode i _ _)) d -> toAcc (acc {pos=After i}) d
  Cpy (TFN (Gnode i)) d -> toAcc (acc {pos=Head i,cgi=i:cgi acc}) d
  Cpy _ d -> toAcc acc d
  _ -> acc
  where
    upOneGroup = acc {pos=After (head (cgi acc)),cgi=tail (cgi acc)}

{-|
Converts accumulated data to OSC message. Check whether node with same id
appear in both of insertions and deletions, if so make n_set, n_map, or n_mapa
messages for the node.
-}
accToOSC :: MsgAcc -> [OSC]
accToOSC (MsgAcc _ _ is ds)
  | IM.null ds = foldl' mkNew [] is'
  | otherwise  = if IM.null ds' then nms ++ ums else nms ++ dms ++ ums
  where
    nms = foldl' mkNew [] is'
    dms = [n_free (IM.keys ds')]
    ums = concat $ IM.elems $ IM.intersectionWithKey assort ds ns
    mkNew os (_,(pos,n)) = case pos of
      After j -> treeToNewWith AddAfter j n ++ os
      Head  j -> treeToNewWith AddToHead j n ++ os
    mkSet (_,n) os = treeToSet n ++ os
    (dupIs, ns') = IM.partitionWithKey (\k _ -> k `IM.member` ds) ns
    ns = IM.fromList is
    is' = filter (\(i,_) -> i `IM.notMember` ds) is
    ds' = IM.difference ds ns

-- | Convert to OSC messages from inserted node and delete nodes.
assort :: Int               -- ^ node id
       -> SCNode            -- ^ Deleted node
       -> (Position,SCNode) -- ^ Inserted node and its position
       -> [OSC]
assort i nd (p,ni) = case (nd,ni) of
  (Group _ _, Group _ _) ->
    let (a,j) = at p in [n_order a j i]
  (Group _ _, Synth _ n _) ->
    let (a,j) = at p in n_free [i]:treeToNewWith a j ni
  (Synth _ _ _, Group _ _) ->
    let (a,j) = at p in [n_free [i],g_new [(i,a,j)]]
  (Synth i1 n1 ps1, Synth i2 n2 ps2)
    | n1 == n2 && ps1 == ps2 ->
      let (a,j) = at p in [n_order a j i]
    | n1 == n2 ->
      let (a,j) = at p in n_order a j i:treeToSet ni
    | otherwise -> let (a,j) = at p in n_free [i]:treeToNewWith a j ni
  where
    at p = case p of Head j -> (AddToHead, j); After j -> (AddAfter, j)

scnId :: SCN -> Int
scnId (Gnode i)     = i
scnId (Snode i _ _) = i

scn2n :: SCN -> SCNode
scn2n (Gnode i) = Group i []
scn2n (Snode i n ps) = Synth i n ps

--
-- Debugging
--

-- | Dump diff and message.
ddm :: SCNode -> SCNode -> IO ()
ddm a b = do
  let d = diffSCN a b
  dumpDiff d
  mapM_ print . accToOSC . toAcc (initialAcc (nodeId a)) $ d

-- | Update root node and then dump the contents.
up :: Transport t => SCNode -> t -> IO ()
up n = sendAsync (updateNode n) printRootNode

-- | Sends given actions in asynchronus manner.
sendAsync :: Transport t => (t -> IO a) -> (t -> IO b) -> t -> IO b
sendAsync act1 act2 fd = do
  sessId <- randomRIO (0,2^16)
  act1 fd
  send fd $ sync sessId
  Message _ [Int replyId] <- wait fd "/synced"
  res <- act2 fd
  when (replyId /= sessId) $ do
    putStrLn "server accessed from other connection"
    putStrLn $ concat ["Sent ", show sessId, " for sync"]
    putStrLn $ concat ["Got ", show replyId, " from server"]
  return res
