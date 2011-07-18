{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Make OSC message with comparing two node tree. The result OSC contains set 
-- of message to modify one node tree to another.
--
-- TODO:
--
-- * Support adding group.
--
-- * Support reordering nodes.
--
-- * Add function to send new node with specifying add target and action.
--
module SCDiff where

import Data.Tree

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import Sound.SC3.Lepton.QuickCheck

import MyDiff
-- import Data.Generic.Diff
import Sample
  
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
                  
-- | OSC guts extracted from EditScript.
data Operation = InsertOp AddAction Int SCNode
               | FreeOp Int
               | SetOp Int [SynthParam]
                 deriving (Eq,Show)
  
-- | Hold where to insert if new node were added.
data Position = Head Int | After Int
                
diffSCN :: Type f (Tree SCN) 
        => SCNode -> SCNode -> EditScript f (Tree SCN) (Tree SCN)
diffSCN ta tb = diff (toRose ta) (toRose tb)

diffMessage :: SCNode -> SCNode -> [OSC]
diffMessage t = concatMap opToOSC . toOp initialPosition . diffSCN t

-- | Head of root node.
initialPosition :: Position
initialPosition = Head 0

{-|

Converts edit script to list of operation. What we want to do here is to 
convert EditScript to set of operation so that easily converted to OSC 
messages.

* When deletion of node comes after insertion of node with same nodeid, 
  operation is updating the node parameter.

* When deletion of node comes after other than insertion of node, operation 
  is deleting the node.

* When insertion of node comes before other than deletion of node, 
  operation is adding new node. We cannot detect this until seeking an
  editscript after finding Ins of node. Also, need to keep track of
  target node id, and position. Note that AddAfter add action could not
  be used when adding to group without containing any node yet.

This function seeks 2 sequence of EditScriptL constructors at once, and 
determine which operation to take.

-}
toOp :: forall txs tys . Position -> EditScriptL TFamily txs tys -> [Operation]
toOp pos d0 = case d0 of
  Ins (TFN (Snode i n ps)) (Del (TFN _) d) -> SetOp i ps : toOp pos d
  Ins (TFN (Snode i n ps)) d        -> newN (Synth i n ps) pos:toOp (After i) d
  Ins (TFN (Gnode i)) d             -> newN (Group i []) pos:toOp (Head i) d
  Ins _ (Del (TFN (Snode i _ _)) d) -> FreeOp i:toOp pos d
  Ins _ (Cpy (TFN node) d)          -> toOp (nextPos node) d
  Ins _ d                           -> toOp pos d
  Cpy _ (Del (TFN n) d)             -> FreeOp (scnId n) : toOp (nextPos n) d
  Cpy _ (Cpy (TFN node) d)          -> toOp (nextPos node) d
  Cpy _ d                           -> toOp pos d
  Del _ (Del (TFN (Snode i _ _)) d) -> FreeOp i : toOp pos d
  Del _ (Cpy (TFN node) d)          -> toOp (nextPos node) d
  Del _ d                           -> toOp pos d
  _                                 -> []
  
opToOSC :: Operation -> [OSC]
opToOSC (FreeOp i) = [n_free [i]]
opToOSC (InsertOp a j n) = treeToNew j n
opToOSC (SetOp i ps) = treeToSet (Synth i "" ps)

-- XXX: 
-- Write SCNode -> [OSC] converting function with specifying AddAction and 
-- Target.
newN :: SCNode -> Position -> Operation
newN node pos = case pos of
  Head j  -> InsertOp AddToHead j node
  After j -> InsertOp AddAfter j node
  
nextPos :: SCN -> Position  
nextPos (Snode i _ _) = After i
nextPos (Gnode i)     = Head i
  
scnId :: SCN -> Int  
scnId (Gnode i)     = i
scnId (Snode i _ _) = i

toRose :: SCNode -> Tree SCN
toRose (Group i ns) = Node (Gnode i) (map toRose ns)
toRose (Synth i n ps) = Node (Snode i n ps) []
