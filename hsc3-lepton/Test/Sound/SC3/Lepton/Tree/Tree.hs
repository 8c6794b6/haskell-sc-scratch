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

import Sound.OSC
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.QuickCheck ()

import qualified Data.ByteString.Char8 as C8

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
  forAll (arbitrary `suchThat` \n -> nodeSize n > 0) $ \n ->
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

nodeToOSC :: SCNode -> Message
nodeToOSC n = Message "/g_queryTree.reply" (Int32 1:f n []) where
  f node os = case node of
    Group i ns      -> Int32 (fromIntegral i):
                       Int32 (fromIntegral $ length ns):foldr f os ns
    Synth i name ps ->
      Int32 (fromIntegral i):Int32 (-1):ASCII_String (C8.pack name):
      Int32 (fromIntegral $ length ps):foldr g [] ps ++ os
  g p ps = case p of
    m := v  -> ASCII_String (C8.pack m) :
               Float (fromRational $ toRational v) : ps
    m :<- v -> ASCII_String (C8.pack m) :
               ASCII_String (C8.pack $ 'c':show v) : ps
    m :<= v -> ASCII_String (C8.pack m) :
               ASCII_String (C8.pack $ 'a':show v) : ps
