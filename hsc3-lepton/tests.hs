------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Arbitrary instance of SCNode and SynthParam for QuickCheck tests.
--
module Main where

import Test.QuickCheck ((==>))
import qualified Test.QuickCheck as Q

import Sound.OpenSoundControl
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as TF

import Sound.SC3.Lepton.Tree
import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.QuickCheck

main :: IO ()
main = defaultMain
  [testGroup "Tree"
   [testProperty "treeToNew" prop_treeToNew
   ,testProperty "treeToSet" prop_treeToSet]
  ,testGroup "Parse"
   [testProperty "parse_datum" prop_parse_datum]]

prop_treeToNew :: SCNode -> Q.Property
prop_treeToNew n = (nodeSize n > 0) ==> (not $ null $ treeToNew 1 n)

prop_treeToSet :: SCNode -> Q.Property
prop_treeToSet n = (paramSize n > 0) ==> (not $ null $ treeToSet n)

prop_parse_datum :: [Datum] -> Q.Property
prop_parse_datum ds =
  (not $ null ds) ==>
  case parse datum ds of
    Right _ -> True
    Left  _ -> False

nodeSize :: SCNode -> Int
nodeSize (Group _ ns) = foldr (\n s -> nodeSize n + s) 1 ns
nodeSize (Synth _ _ _) = 1

paramSize :: SCNode -> Int
paramSize (Group _ ns) = foldr (\n s -> paramSize n + s) 0 ns
paramSize (Synth _ _ ps) = length ps
