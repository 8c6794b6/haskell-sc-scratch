{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
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
module Test.Sound.SC3.Lepton.Tree.Diff where

import Control.Applicative
import Data.Map (Map)
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Data.Generics.Uniplate.Operations
import Sound.OSC
import Sound.SC3 hiding (label)

import Sound.SC3.Lepton.QuickCheck
import Sound.SC3.Lepton.Tree.Tree
import Sound.SC3.Lepton.Tree.Diff
import Sound.SC3.Lepton.Tree.Zipper

import qualified Data.Map as M


prop_diff_insert :: SCNode -> Property
prop_diff_insert n1 = do
  n0 <- gen_uniqueIdNode
  let z = SCZipper n0 []
  forAll (elements (nodeIds n0) `suchThat` (/= (nodeId n0))) $ \targetId ->
    let n0' = focus $ goTop (insert' n1 (Just (AddAfter,targetId)) z)
        msgs = diffMessage n0 n0'
    in  all (isMsg "/s_new" ||? isMsg "/g_new" ||? isMsg "/n_map" ||?
             isMsg "/n_mapa") msgs

prop_diff_delete :: Property
prop_diff_delete =
  forAll gen_uniqueIdNode $ \n0 ->
    forAll (elements (nodeIds n0)) $ \idToRemove ->
    let n0' = focus $ goTop (delete idToRemove z)
        msgs = diffMessage n0 n0'
        z = SCZipper n0 []
    in  all (isMsg "/n_free") msgs

prop_diff_update :: Property
prop_diff_update =
  forAll gen_uniqueIdNode $ \n0 ->
  forAll (elements (nodeIds n0)) $ \idToModify ->
  forAll (arbitrary `suchThat` (not . null)) $ \ps ->
  let up n@(Synth i _ _) | i == idToModify = updateParams ps n
      up n               = n
      n0' = transformBi up n0
      msgs = diffMessage n0 n0'
  in  all (isMsg "/n_map" ||? isMsg "/n_mapa" ||? isMsg "/n_set" ||?
           isMsg "/n_order") msgs ||
      null msgs

prop_diff_mixed :: Property
prop_diff_mixed = do
  n0 <- gen_uniqueIdNode `suchThat` (\n -> not $ 0 `elem` nodeIds n)
  n1 <- gen_uniqueIdNode `suchThat` (\n -> not $ 0 `elem` nodeIds n)
  let msgs = diffMessage (Group 0 [n0]) (Group 0 [n1])
  collect (countOccurence $ map msgString msgs) $ length msgs >= 0

countOccurence :: [String] -> String
countOccurence = f . foldr ($) M.empty . map (\k -> M.insertWith' (+) k 1)
  where f = M.foldrWithKey (\k a bs -> k ++ ":" ++ show a ++ " " ++ bs) ""

isGroup :: SCNode -> Bool
isGroup (Group _ _) = True
isGroup _           = False

isMsg :: String -> Message -> Bool
isMsg str (Message m _) = m == str

msgString :: Message -> String
msgString (Message m _) = m

(||?) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(||?) = liftBiFunc (||)

liftBiFunc :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
liftBiFunc q f g = \n -> f n `q` g n

gen_uniqueIdNode :: Gen SCNode
gen_uniqueIdNode = arbitrary `suchThat` (\n -> hasUniqueIds n && isGroup n)

tests :: TestTree
tests = $testGroupGenerator
