------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Main tests.
--
module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck ((==>), (.||.))
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as QM

import Sound.OpenSoundControl

import Sound.SC3.Lepton.Tree
import Sound.SC3.Lepton.Parser
import Sound.SC3.Lepton.QuickCheck

main :: IO ()
main = do
  results <- mapM Q.quickCheckResult
    [Q.label "treeToNew" prop_treeToNew
    ,Q.label "treeToSet" prop_treeToSet
    ,Q.label "drawSCNode" prop_drawSCNode
    ,Q.label "arbitrary_Datum" prop_arbitrary_Datum
    ,Q.label "arbitrary_Time" prop_arbitrary_Time
    ,Q.label "parse_datum" prop_parse_datum
    ,Q.label "parse_any_datum" prop_parse_any_datum
    ,Q.label "parseNode" prop_parseNode]
  if any (not . isSuccess) results
     then exitFailure
     else exitSuccess

isSuccess :: Q.Result -> Bool
isSuccess r = case r of Q.Success _ _ _ -> True; _ -> False

prop_arbitrary_Datum :: Datum -> Bool
prop_arbitrary_Datum n = n == n

prop_arbitrary_Time :: Time -> Bool
prop_arbitrary_Time n = n == n

prop_treeToNew :: SCNode -> Q.Property
prop_treeToNew n =
  (nodeSize n > 0) ==>
  (not $ null $ treeToNew 1 n)

prop_treeToSet :: SCNode -> Q.Property
prop_treeToSet n =
  (paramSize n > 0) ==>
  (not $ null $ treeToSet n)

prop_drawSCNode :: SCNode -> Q.Property
prop_drawSCNode n =
  (nodeSize n > 1) ==>
  (length (drawSCNode n) > 1)

prop_parseNode :: SCNode -> Q.Property
prop_parseNode n =
  let isGroup x = case x of Group _ _ -> True; _ -> False
  in  isGroup n ==>
      parseNode (nodeToOSC n) == n

prop_parse_datum :: [Datum] -> Q.Property
prop_parse_datum ds =
  (not $ null ds) ==>
  either (const False) (const True) (parse datum ds)

prop_parse_any_datum :: [Datum] -> Q.Property
prop_parse_any_datum ds =
  let isRight x = case x of Right _ -> True; _ -> False
  in (not $ null ds) ==>
     (isRight $ parse int ds) .||.
     (isRight $ parse float ds) .||.
     (isRight $ parse double ds) .||.
     (isRight $ parse string ds) .||.
     (isRight $ parse blob ds) .||.
     (isRight $ parse timeStamp ds) .||.
     (isRight $ parse midi ds)

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

nodeSize :: SCNode -> Int
nodeSize (Group _ ns) = foldr (\n s -> nodeSize n + s) 1 ns
nodeSize (Synth _ _ _) = 1

paramSize :: SCNode -> Int
paramSize (Group _ ns) = foldr (\n s -> paramSize n + s) 0 ns
paramSize (Synth _ _ ps) = length ps
