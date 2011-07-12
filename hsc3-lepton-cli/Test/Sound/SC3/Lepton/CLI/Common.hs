module Test.Sound.SC3.Lepton.CLI.Common where

import Test.QuickCheck

import Control.Applicative
import Data.Generics.Uniplate.Operations
import Sound.SC3
import Sound.SC3.Lepton
import Sound.SC3.Lepton.QuickCheck

import Sound.SC3.Lepton.CLI.Parser
import Sound.SC3.Lepton.CLI.SCShellCmd
import Sound.SC3.Lepton.CLI.SCZipper

import qualified Data.IntSet as IS

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

instance Arbitrary SCZipper where
  arbitrary = SCZipper <$> arbitrary <*> arbitrary

instance Arbitrary SCPath where
  arbitrary = SCPath <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AddAction where
  arbitrary = elements [AddToHead .. AddReplace]

instance CoArbitrary SynthParam where
  coarbitrary (n:=v) =  variant 0 . coarbitrary n . coarbitrary v
  coarbitrary (n:<-v) = variant 0 . coarbitrary n . coarbitrary v
  coarbitrary (n:<=v) = variant 0 . coarbitrary n . coarbitrary v

instance CoArbitrary SCNode where
  coarbitrary (Synth i n ps) =
    variant 0 . coarbitrary i . coarbitrary n . coarbitrary ps
  coarbitrary (Group i ns) =
    variant 1 . coarbitrary i . coarbitrary ns

instance CoArbitrary SCPath where
  coarbitrary (SCPath n ls rs) =
    variant 2 . coarbitrary n . coarbitrary ls . coarbitrary rs

instance CoArbitrary SCZipper where
  coarbitrary (SCZipper n ps) =
    variant 3 . coarbitrary n . coarbitrary ps

instance Arbitrary Step where
  arbitrary = oneof [return GoUp, return GoTop, GoDown <$> arbitrary]

instance CoArbitrary Step where
  coarbitrary s = case s of
    GoUp     -> variant 0
    GoTop    -> variant 1
    GoDown n -> variant 2 . coarbitrary n

instance Arbitrary Cmd where
  arbitrary = oneof
    [Ls <$> arbitrary
    ,Cd <$> arbitrary
    ,Mv <$> arbitrary <*> arbitrary <*> arbitrary
    ,Tree <$> arbitrary
    ,return Status
    ,return Refresh
    ,Set <$> arbitrary <*> arbitrary
    ,Run <$> arbitrary
    ,Free <$> arbitrary
    ,New <$> arbitrary <*> arbitrary]

instance CoArbitrary Cmd where
  coarbitrary c = case c of
    Ls ps -> variant 1 . coarbitrary ps
    _     -> undefined

isSuccess :: Result -> Bool
isSuccess r = case r of Success _ _ _ -> True; _ -> False;

isSynth :: SCNode -> Bool
isSynth n = case n of Synth _ _ _ -> True; _ -> False;

isGroup :: SCNode -> Bool
isGroup n = case n of Group _ _ -> True; _ -> False;

nodeIdOfPath :: SCPath -> NodeId
nodeIdOfPath (SCPath n _ _) = n

leftPaths :: SCPath -> [SCNode]
leftPaths (SCPath _ ls _) = ls

rightPaths :: SCPath -> [SCNode]
rightPaths (SCPath _ _ rs) = rs

gen_uniqueNodeId :: Gen SCNode
gen_uniqueNodeId = arbitrary `suchThat` hasUniqueIds

gen_uniqueZipper :: Gen SCZipper
gen_uniqueZipper = arbitrary `suchThat` hasUniqueNodes where

hasUniqueNodes :: SCZipper -> Bool
hasUniqueNodes (SCZipper n ps) = uniqueIds (nodeIds n ++ concatMap fp ps) where
  fp :: SCPath -> [Int]
  fp (SCPath n ls rs) = n : concatMap nodeIds (ls ++ rs)
  uniqueIds :: [Int] -> Bool
  uniqueIds ids = IS.size (IS.fromList ids) == length ids

hasUniqueIds :: SCNode -> Bool
hasUniqueIds n = listSize == setSize where
  setSize = IS.size . IS.fromList $ l
  listSize = length l
  l = universe n >>= \n' -> case n' of Group k _ -> [k]; Synth k _ _ -> [k]

nodeIds :: SCNode -> [Int]
nodeIds n = do
  n' <- universe n
  case n' of
    Synth nid _ _ -> [nid]
    Group nid _   -> [nid]

nodeIdsInZipper :: SCZipper -> [Int]
nodeIdsInZipper (SCZipper n ps) = nodeIds n ++ concatMap nodeIdsInPath ps

nodeIdsInPath :: SCPath -> [Int]
nodeIdsInPath (SCPath n ls rs) = n:concatMap nodeIds ls ++ concatMap nodeIds rs
