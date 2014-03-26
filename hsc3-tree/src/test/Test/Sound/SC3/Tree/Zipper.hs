{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
module Test.Sound.SC3.Tree.Zipper where

import Control.Applicative
import Text.Show.Functions ()

import Sound.SC3 hiding (label)

import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
import Test.Tasty.TH (testGroupGenerator)
import Test.QuickCheck

import Sound.SC3.Tree.Type
import Sound.SC3.Tree.Zipper
import Test.Sound.SC3.Tree.QuickCheck

import qualified Data.IntSet as IS

prop_updateNode :: (SCNode -> SCNode) -> SCZipper -> Property
prop_updateNode f z =
  let newSize = sizeOf (updateNode f z)
      oldSize = sizeOf z
  in  collect (newSize `compare` oldSize) $ True

prop_zipperInstance :: Property
prop_zipperInstance =
  forAll (arbitrary :: Gen SCZipper) $ \z -> z == z && show z == show z

prop_steps :: [Step] -> SCZipper -> Property
prop_steps s z =
  not (null s) ==>
  let z' = steps s z
  in  sizeOf (focus z') >= 0 || sizeOf (focus z') < 0

prop_goTop :: SCZipper -> Property
prop_goTop z =
  not (null $ scPaths z) ==>
  forAll (elements $ scPaths z) $ \p ->
  focus (goTop z) == focus (goTop (goDown (nodeIdOfPath p) z))

prop_delete :: SCZipper -> Property
prop_delete z =
  not (null $ scPaths z) ==>
  forAll (elements $ scPaths z) $ \p ->
  not (null $ leftPaths p ++ rightPaths p) ==>
  forAll (elements $ leftPaths p ++ rightPaths p) $ \n ->
  sizeOf z > sizeOf (delete (nodeId n) z)

prop_insert :: AddAction -> Property
prop_insert a =
  forAll (gen_uniqueZipper `suchThat` (not . null . scPaths)) $ \z ->
  forAll gen_uniqueNodeId $ \n ->
  forAll (elements $ nodeIdsInZipper z) $ \targetId ->
    IS.size (IS.fromList (nodeIdsInZipper z) `IS.intersection`
             IS.fromList (nodeIds n)) == 0 ==>
    nodeIdOfPath (head (scPaths z)) /= targetId ==>
    collect a $
    let z' = insert' n (Just (a,targetId)) z
    in case a of
      AddReplace ->
        sizeOf (nodeById targetId z) + sizeOf z' == sizeOf n + sizeOf z
      _          ->
        (sizeOf z + sizeOf n == sizeOf z') || (z == z')

-- | Insert and then delete the same node. This is doing almost same thing
-- as @move@ function with AddToTail add action.
prop_insert_delete :: Property
prop_insert_delete =
  forAll gen_uniqueZipper $ \z ->
  forAll gen_uniqueNodeId $ \n ->
  classify (isSynth n) "synth" $
  classify (isGroup n) "group" $
  let z' = insert n z
      everyNodesAreUnique =
        IS.size (IS.fromList (f z) `IS.intersection`
                 IS.fromList (nodeIds n)) == 0
      f (SCZipper n' ps) = nodeIds n' ++ concatMap g ps
      g (SCPath n' ls rs) = n' : concatMap nodeIds ls ++ concatMap nodeIds rs
  in  everyNodesAreUnique ==> z == delete (nodeId n) z'

class Sizeable n where
  sizeOf :: n -> Int

instance Sizeable SCZipper where
  sizeOf (SCZipper n ps) = sizeOf n + sizeOf ps

instance Sizeable SCPath where
  sizeOf (SCPath _ ls rs) = 1 + sizeOf ls + sizeOf rs

instance Sizeable SCNode where
  sizeOf (Group _ ns) = 1 + sizeOf ns
  sizeOf (Synth _ _ _) = 1

instance Sizeable a => Sizeable [a] where
  sizeOf = sum . map sizeOf

nodeIdOfPath :: SCPath -> NodeId
nodeIdOfPath (SCPath n _ _) = n

leftPaths :: SCPath -> [SCNode]
leftPaths (SCPath _ ls _) = ls

rightPaths :: SCPath -> [SCNode]
rightPaths (SCPath _ _ rs) = rs

gen_uniqueNodeId :: Gen SCNode
gen_uniqueNodeId = arbitrary `suchThat` hasUniqueIds

gen_uniqueZipper :: Gen SCZipper
gen_uniqueZipper = arbitrary `suchThat` hasUniqueNodes

hasUniqueNodes :: SCZipper -> Bool
hasUniqueNodes (SCZipper n ps) = uniqueIds (nodeIds n ++ concatMap fp ps) where
  fp :: SCPath -> [Int]
  fp (SCPath n ls rs) = n:concatMap nodeIds (ls ++ rs)
  uniqueIds :: [Int] -> Bool
  uniqueIds ids = IS.size (IS.fromList ids) == length ids

nodeIdsInZipper :: SCZipper -> [Int]
nodeIdsInZipper (SCZipper n ps) = nodeIds n ++ concatMap nodeIdsInPath ps

nodeIdsInPath :: SCPath -> [Int]
nodeIdsInPath (SCPath n ls rs) = n:concatMap nodeIds ls ++ concatMap nodeIds rs

-- | SCZipper test data gets too large for prop_goTop
-- modify maxSize to 25 from default, which is 100.
tests :: TestTree
tests = localOption (QuickCheckMaxSize 25) ($testGroupGenerator)

isSynth :: SCNode -> Bool
isSynth n = case n of Synth _ _ _ -> True; _ -> False

isGroup :: SCNode -> Bool
isGroup x = case x of Group _ _ -> True; _ -> False
