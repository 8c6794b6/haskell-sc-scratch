{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- 2nd example of gdiff.
--

module GDiffScratch2 where

-- import Data.Generic.Diff
import MyDiff
import Data.Tree
import Sound.OpenSoundControl
import Sound.SC3.Lepton
import Sound.SC3.Lepton.QuickCheck
import Test.QuickCheck

import Sample

data SCNodeFamily :: * -> * -> * where  
  SCNSynth       :: 
    SCNodeFamily SCNode (Cons Int (Cons String (Cons [SynthParam] Nil)))
  SCNGroup       :: SCNodeFamily SCNode (Cons Int (Cons [SCNode] Nil))
  SCNNil         :: SCNodeFamily [SCNode] Nil
  SCNCons        :: SCNodeFamily [SCNode] (Cons SCNode (Cons [SCNode] Nil))
  SCNInt         :: Int -> SCNodeFamily Int Nil
  SCNString      :: String -> SCNodeFamily String Nil
  SCNParams      :: [SynthParam] -> SCNodeFamily [SynthParam] Nil
  
instance Show (SCNodeFamily a b) where  
  show SCNSynth       = "Synth"
  show SCNGroup       = "Group"
  show SCNNil         = "[]"
  show SCNCons        = ":"
  show (SCNInt i)     = show i
  show (SCNString s)  = s
  show (SCNParams ps) = show ps
                                 
instance Family SCNodeFamily where 
  decEq SCNSynth SCNSynth = Just (Refl,Refl)
  decEq SCNGroup SCNGroup = Just (Refl,Refl)
  decEq SCNNil SCNNil     = Just (Refl,Refl)
  decEq SCNCons SCNCons   = Just (Refl,Refl)
  decEq (SCNInt a) (SCNInt b) | a == b    = Just (Refl,Refl)
                              | otherwise = Nothing
  decEq (SCNString a) (SCNString b) | a == b    = Just (Refl,Refl)
                                      | otherwise = Nothing
  decEq (SCNParams a) (SCNParams b) | a == b     = Just (Refl,Refl)
                                      | otherwise = Nothing
                                      | otherwise = Nothing
  decEq _ _ = Nothing
  
  fields SCNSynth (Synth i n ps) = Just (CCons i (CCons n (CCons ps CNil)))
  fields SCNGroup (Group i ns)    = Just (CCons i (CCons ns CNil))
  fields SCNNil []                  = Just CNil
  fields SCNCons (n:ns)            = Just (CCons n (CCons ns CNil))
  fields (SCNInt _) _               = Just CNil
  fields (SCNString _) _            = Just CNil
  fields (SCNParams _) _            = Just CNil
  fields _ _                        = Nothing

  apply SCNSynth (CCons i (CCons n (CCons ps CNil))) = Synth i n ps
  apply SCNGroup (CCons i (CCons ns CNil))            = Group i ns
  apply SCNNil CNil                                     = []
  apply SCNCons (CCons n (CCons ns CNil))             = n:ns
  apply (SCNInt i) CNil                                = i
  apply (SCNString n) CNil                             = n
  apply (SCNParams p) CNil                             = p

  string = show

instance Type SCNodeFamily SCNode where
  constructors = [Concr SCNSynth, Concr SCNGroup]

instance Type SCNodeFamily [SCNode] where
  constructors = [Concr SCNNil, Concr SCNCons]
  
instance Type SCNodeFamily [SynthParam] where
  constructors = [Abstr SCNParams]
  
instance Type SCNodeFamily String where  
  constructors = [Abstr SCNString] 
  
instance Type SCNodeFamily Int where  
  constructors = [Abstr SCNInt] 

-- | Accumurate string representation of EditScript to list.
edits :: forall f txs tys . EditScriptL f txs tys -> [String] -> [String]
edits (Ins c d) acc   = ("Ins " ++ string c) : edits d acc
edits (Del c d) acc   = ("Del " ++ string c) : edits d acc
edits (Cpy c d) acc   = ("Cpy " ++ string c) : edits d acc
edits (CpyTree d) acc = "CpyTree" : edits d acc
edits _ acc           = acc

updateMessage :: forall f txs tys . EditScriptL f txs tys -> [OSC]
updateMessage = undefined

type SCNodeDiff = EditScript SCNodeFamily SCNode SCNode

diffSCNode :: SCNode -> SCNode -> SCNodeDiff
diffSCNode = diff

prop_diffNode :: SCNode -> SCNode -> Property
prop_diffNode n1 n2 = do
  let d = diffSCNode t1 t2
  label "patching" $ length (edits d []) >= 0
  
headOf (Ins _ _)   = "Ins"  
headOf (Del _ _)   = "Del"
headOf (Cpy _ _)   = "Cpy"
headOf (CpyTree _) = "CpyTree"
headOf End         = "End"

main = do
  -- putStrLn $ "Num operations: " ++ 
  --   show (length $ edits (diffSCNode t1 t2) [])
  print (diffSCNode t1 t2)    
  
{-
     407,582,880 bytes allocated in the heap
     166,172,192 bytes copied during GC
      11,758,824 bytes maximum residency (15 sample(s))
       1,589,376 bytes maximum slop
              35 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:   763 collections,     0 parallel,  0.21s,  0.21s elapsed
  Generation 1:    15 collections,     0 parallel,  0.26s,  0.26s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.52s  (  0.52s elapsed)
  GC    time    0.46s  (  0.46s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.98s  (  0.98s elapsed)

  %GC time      47.2%  (47.3% elapsed)

  Alloc rate    786,361,343 bytes per MUT second

  Productivity  52.6% of total user, 52.6% of total elapsed

-}

{- 

Modified 'steps' and 'bestSteps' function not to use Nat data type. 
Used bangpatterned Int instead.

@@
   1,115,358,200 bytes allocated in the heap
     107,766,560 bytes copied during GC
      23,096,176 bytes maximum residency (7 sample(s))
         444,048 bytes maximum slop
              48 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:  2120 collections,     0 parallel,  0.33s,  0.33s elapsed
  Generation 1:     7 collections,     0 parallel,  0.19s,  0.19s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    1.61s  (  1.61s elapsed)
  GC    time    0.52s  (  0.52s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    2.13s  (  2.13s elapsed)

  %GC time      24.6%  (24.6% elapsed)

  Alloc rate    693,988,073 bytes per MUT second

  Productivity  75.3% of total user, 75.3% of total elapsed
@@

Use Nat data type with Zero and Succ constructor, and bang patterns in 'steps' 
and 'bestSteps' function.

@@
     610,300,616 bytes allocated in the heap
     317,107,000 bytes copied during GC
      29,915,960 bytes maximum residency (12 sample(s))
       2,468,808 bytes maximum slop
              80 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:  1152 collections,     0 parallel,  0.44s,  0.44s elapsed
  Generation 1:    12 collections,     0 parallel,  0.46s,  0.46s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    1.14s  (  1.14s elapsed)
  GC    time    0.90s  (  0.90s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    2.04s  (  2.04s elapsed)

  %GC time      44.0%  (44.1% elapsed)

  Alloc rate    534,121,999 bytes per MUT second

  Productivity  55.9% of total user, 55.8% of total elapsed
@@


-}