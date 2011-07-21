------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Test.Sound.SC3.Lepton.Tree.Tree where

import Test.QuickCheck

import Sound.OpenSoundControl
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.QuickCheck

import Test.Sound.SC3.Lepton.Common

tests :: [Property]
tests = 
  [label "treeToNew" prop_treeToNew
  ,label "treeToSet" prop_treeToSet
  ,label "drawSCNode" prop_drawSCNode
  ,label "parseNode" prop_parseNode]

prop_treeToNew :: Property
prop_treeToNew = 
  forAll (arbitrary `suchThat` \n -> nodeSize n > 0) $ \n ->
  not $ null $ treeToNew 1 n

prop_treeToSet :: Property
prop_treeToSet =
  forAll (arbitrary `suchThat` \n -> paramSize n > 0) $ \n -> 
  not $ null $ treeToSet n

prop_drawSCNode :: Property
prop_drawSCNode =
  forAll (arbitrary `suchThat` \n -> nodeSize n >0) $ \n -> 
  length (drawSCNode n) > 1
  
prop_parseNode :: Property
prop_parseNode =
  forAll (arbitrary `suchThat` isGroup) $ \n -> 
  parseNode (nodeToOSC n) == n

nodeSize :: SCNode -> Int
nodeSize (Group _ ns) = foldr (\n s -> nodeSize n + s) 1 ns
nodeSize (Synth _ _ _) = 1

paramSize :: SCNode -> Int
paramSize (Group _ ns) = foldr (\n s -> paramSize n + s) 0 ns
paramSize (Synth _ _ ps) = length ps

nodeToOSC :: SCNode -> OSC
nodeToOSC n = Message "/g_queryTree.reply" (Int 1:f n []) where
  f node os = case node of
    Group i ns -> Int i:Int (length ns):foldr f os ns
    Synth i name ps ->
      Int i:Int (-1):String name:Int (length ps):foldr g [] ps ++ os
  g p ps = case p of
    n := v  -> String n : Float v : ps
    n :<- v -> String n : String ('c':show v) : ps
    n :<= v -> String n : String ('a':show v) : ps


