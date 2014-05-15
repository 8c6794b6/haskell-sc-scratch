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
module Test.Sound.SC3.Tree.Diff where

import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Sound.OSC
import Sound.SC3 hiding (label)

import Sound.SC3.Tree.Type
import Sound.SC3.Tree.Diff
import Sound.SC3.Tree.Zipper

import Test.Sound.SC3.Tree.QuickCheck ()

import qualified Data.Map as M

-- Test a bug found when patching empty groups to root node.
case_diff_n0 :: Assertion
case_diff_n0 = do
  let n0 = Group 0
           [ Group 1 []
           , Group 2 [] ]
      n1 = Group 0
           [ Group 1
             [ Group 10 []
             , Group 11 [] ]
           , Group 2 [] ]
  diffMessage n0 n1 @=? [g_new [(10,AddToHead,1)], g_new [(11,AddAfter,10)]]

-- Test a bug found when replacing entire contents of group.
case_diff_n1 :: Assertion
case_diff_n1 = do
    let n0 = Group 101
             [ Synth 1616049966 "sin02"
               ["out":=18]
             , Synth 1777095663 "ap01"
               ["in":=18,"out":=18]
             , Synth 1425562103 "cmb02"
               ["in":=18,"out":=18]
             , Synth 10199 "router"
               ["in":=18,"out":=16] ]
        n1 = Group 101
            [ Synth 53822416 "sin03"
              ["out":=18]
            , Synth 112652199 "ap02"
              ["in":=18,"out":=18]
            , Synth 690662386 "lp01"
              ["out":=18,"in":=18]
            , Synth 10199 "router"
              ["in":=18,"out":=16] ]
    diffMessage n0 n1 @=?
        [ s_new "sin03" 53822416 AddToHead 101
          [("out",18)]
        , s_new "ap02" 112652199 AddAfter 53822416
          [("in",18),("out",18)]
        , s_new "lp01" 690662386 AddAfter 112652199
          [("out",18),("in",18)]
        , n_free [1425562103,1616049966,1777095663] ]

prop_diff_insert :: SCNode -> Property
prop_diff_insert n1 =
  forAll gen_uniqueIdNode $ \n0 -> do
      let z = SCZipper n0 []
      forAll (elements (nodeIds n0) `suchThat` (/= nodeId n0)) $ \targetId ->
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
      n0'  = mapSCNode up n0
      msgs = diffMessage n0 n0'
  in  all (isMsg "/n_map" ||? isMsg "/n_mapa" ||? isMsg "/n_set" ||?
           isMsg "/n_order") msgs ||
      null msgs

prop_diff_mixed :: Property
prop_diff_mixed =
   let gen_nonRootNode =
           gen_uniqueIdNode `suchThat` (\n -> not $ 0 `elem` nodeIds n)
   in  forAll gen_nonRootNode $ \n0 ->
       forAll gen_nonRootNode $ \n1 ->
           let msgs = diffMessage (Group 0 [n0]) (Group 0 [n1])
           in  collect (countOccurence $ map msgString msgs) (length msgs >= 0)

countOccurence :: [String] -> String
countOccurence =
    M.foldrWithKey (\k a bs -> k ++ ":" ++ show a ++ " " ++ bs) "" .
    foldr ($) M.empty . map (\k -> M.insertWith' (+) k (1::Int))

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
