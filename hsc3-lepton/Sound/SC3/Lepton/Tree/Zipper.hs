{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Zipper data types and functions for SCNode.
--
module Sound.SC3.Lepton.Tree.Zipper
 ( SCZipper(..)
 , SCPath(..)
 , NodePath
 , Step(..)
 , goUp
 , goDown
 , goTop
 , step
 , steps
 , updateNode
 , delete
 , insert
 , insert'
 , move
 , nodeById
 ) where

import Data.List (nub)

import Data.Data
import Data.Generics.Uniplate.Data

import Sound.SC3
import Sound.SC3.Lepton.Tree.Tree

-- | Track current position and data to reconstruct the entire tree
data SCZipper = SCZipper {
    -- | Current focused position.
    focus :: SCNode
    -- | Tracked elements in tree.
  , scPaths :: [SCPath]
  } deriving (Eq, Show, Data, Typeable)

-- | Data type to track information of movement in tree.
data SCPath = SCPath NodeId [SCNode] [SCNode]
              deriving (Eq, Show, Data, Typeable)

-- | Synonym for functions modifying sczipper to zipper.
type NodePath = SCZipper -> SCZipper

-- | Data representation of movement in zipper.
data Step = GoUp | GoTop | GoDown Int
          deriving (Eq, Show)

-- | Returns a function with combining given steps.
steps :: [Step] -> NodePath
steps = foldl (\a b -> step b . a) id

-- | Convert move data representation to function.
step :: Step -> NodePath
step d = case d of
  GoUp     -> goUp
  GoTop    -> goTop
  GoDown n -> goDown n

-- | Go one leven up from current focus.
goUp :: SCZipper -> SCZipper
goUp z@(SCZipper n ps) = case ps of
  []                  -> z
  SCPath nid ls rs:cs -> SCZipper (Group nid (ls++[n]++rs)) cs

-- | Move in to given node id.
goDown :: NodeId -> SCZipper -> SCZipper
goDown _   z@(SCZipper (Synth _ _ _) _) = z
goDown nid z@(SCZipper (Group g ns) cs)
  | noSuchNode = z
  | otherwise  = SCZipper n (SCPath g ls rs:cs)
  where
    noSuchNode = null $ nub $ filter isSeekingNode ns
    (ls, n:rs) = break isSeekingNode ns
    isSeekingNode = (nid ==) . nodeId

-- | Go to top node
goTop :: SCZipper -> SCZipper
goTop z@(SCZipper _ ps)
  | null ps = z
  | otherwise = goTop $ goUp z

-- | Apply given function to focused node
updateNode :: (SCNode -> SCNode) -> SCZipper -> SCZipper
updateNode f (SCZipper n ns) = SCZipper (f n) ns

-- | Remove node
delete :: NodeId -> SCZipper -> SCZipper
delete nid (SCZipper g ps) = SCZipper g' ps' where
  g' = case g of
    Group i ns -> Group i $ f ns
    _          -> g
  ps' = map (\(SCPath i ls rs) -> SCPath i (f ls) (f rs)) ps
  f = foldr removeNid []
  removeNid n ns =
    if nodeId n == nid then ns else
      case n of
        Group i ns' -> Group i (f ns') : ns
        _           -> n : ns

-- | Move given node to new position with given AddAction.
move :: Int -> AddAction -> Int -> SCZipper -> SCZipper
move sourceId action targetId z =
  insert' (nodeById sourceId z) (Just (action,targetId)) $
  delete sourceId z

-- | Get node from zipper by id.
nodeById :: Int -> SCZipper -> SCNode
nodeById nid (SCZipper n ps) = head $ f n ++ concatMap appP ps
  where
    appP :: SCPath -> [SCNode]
    appP (SCPath i ls rs) =
      if i == nid
         then [Group i (ls ++ [n] ++ rs)]
         else concatMap f ls ++ concatMap f rs
    f :: SCNode -> [SCNode]
    f x = if nodeId x == nid then [x] else
            case x of
              Group _ ns -> concatMap f ns
              _          -> []

-- | Insert node as last element under current focused group.
insert :: SCNode -> SCZipper -> SCZipper
insert n z = insert' n Nothing z

-- | Insert new node with given AddAction and target node id.
-- insert' :: SCNode -> AddAction -> Int -> SCZipper -> SCZipper
insert' :: SCNode -> Maybe (AddAction,Int) -> SCZipper -> SCZipper
insert' node Nothing z                =
  insert' node (Just (AddToTail, nodeId $ focus z)) z
insert' node (Just (action,target)) z =
  case action of
    AddToTail  -> transform toTail z
    AddToHead  -> transform toHead z
    AddAfter   -> transform after z
    AddBefore  -> transform before z
    AddReplace -> transform replace z
  where
    foldLRs f =
      map (\(SCPath i ls rs) -> SCPath i (foldr f [] ls) (foldr f [] rs))
    toTail (SCZipper g ps) = SCZipper (f g) (map app ps) where
      app (SCPath i ls rs) = if i == target then
                                 SCPath i ls (rs++[node])
                               else
                                 SCPath i (map f ls) (map f rs)
      f n = case n of
        Group gid ns -> if gid == target then
                          Group gid (ns++[node])
                        else
                          Group gid (map f ns)
        _            -> n
    toHead (SCZipper g ps) = SCZipper (f g) (map app ps) where
      app (SCPath i ls rs) = if i == target then
                                 SCPath i (node:ls) rs
                             else
                                 SCPath i (map f ls) (map f rs)
      f n = case n of
        Group gid ns -> if gid == target then
                          Group gid (node:ns)
                        else
                          Group gid (map f ns)
        _            -> n
    after (SCZipper g ps) = SCZipper (f g) (foldLRs f' ps) where
      f (Group gid ns) = Group gid $ foldr f' [] ns
      f n              = n
      f' x xs = if nodeId x == target then
                  x:node:xs
                else case x of
                  Synth _ _ _ -> x:xs
                  Group _ _   -> f x:xs
    before (SCZipper g ps) = SCZipper (f g) (foldLRs f' ps) where
      f (Group gid ns) = Group gid $ foldr f' [] ns
      f n              = n
      f' x xs = if nodeId x == target then
                  node:x:xs
                else case x of
                  Synth _ _ _ -> x:xs
                  Group _ _   -> f x:xs
    replace (SCZipper g ps) = SCZipper (f g) (map app ps) where
      app (SCPath i ls rs) = SCPath i (map f ls) (map f rs)
      f node' = if nodeId node' == target then
                  node
                else case node' of
                  Group gid ns -> Group gid $ map f ns
                  _            -> node'
