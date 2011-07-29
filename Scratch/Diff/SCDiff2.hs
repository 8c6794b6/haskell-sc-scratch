{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-|

Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Yet another take to compare synth nodes.

Attempt to construct osc message from diff with using zipper and
moving around tree.  Not working.

-}
module SCDiff2 where

import Data.Tree
import Control.Monad
import Control.Monad.Writer

import Data.Tree.Zipper
import Sound.OpenSoundControl
import Sound.SC3.Lepton

import MyDiff
import Sample

data NodeType
  = Grp Int
  | Syn Int String
  | Par [SynthParam]
  deriving (Eq, Show)

toR :: SCNode -> Tree NodeType
toR (Group i ns) = Node (Grp i) (map toR ns)
toR (Synth i n ps) = Node (Syn i n) [Node (Par ps) []]

data SFamily :: * -> * -> * where
  SFNode     :: SFamily (Tree NodeType) (Cons NodeType (Cons [Tree NodeType] Nil))
  SFNodeNil  :: SFamily [Tree NodeType] Nil
  SFNodeCons :: SFamily [Tree NodeType] (Cons (Tree NodeType) (Cons [Tree NodeType] Nil))
  SFN        :: NodeType -> SFamily NodeType Nil

instance Show (SFamily a b) where
  show SFNode     = "Node"
  show SFNodeNil  = "[]"
  show SFNodeCons = ":"
  show (SFN n)    = show n

instance Family SFamily where
  decEq SFNode SFNode               = Just (Refl,Refl)
  decEq SFNodeNil SFNodeNil         = Just (Refl,Refl)
  decEq SFNodeCons SFNodeCons       = Just (Refl,Refl)
  decEq (SFN a) (SFN b) | a == b    = Just (Refl,Refl)
                        | otherwise = Nothing
  decEq _ _ = Nothing

  fields SFNode (Node x ts) = Just (CCons x (CCons ts CNil))
  fields SFNodeNil []       = Just CNil
  fields SFNodeCons (x:xs)  = Just (CCons x (CCons xs CNil))
  fields (SFN _) _          = Just CNil
  fields _ _                = Nothing

  apply SFNode (CCons x (CCons ts CNil))     = Node x ts
  apply SFNodeNil CNil                       = []
  apply SFNodeCons (CCons x (CCons xs CNil)) = x:xs
  apply (SFN n) CNil                         = n

  string = show

instance Type SFamily (Tree NodeType) where
  constructors = [Concr SFNode]

instance Type SFamily [Tree NodeType] where
  constructors = [Concr SFNodeNil, Concr SFNodeCons]

instance Type SFamily NodeType where
  constructors = [Abstr SFN]

ddf :: SCNode -> SCNode -> IO ()
ddf a b = dumpSFDiff (diff (toR a) (toR b) :: EditScript SFamily (Tree NodeType) (Tree NodeType))

dumpSFDiff :: forall f txs tys . EditScriptL f txs tys -> IO ()
dumpSFDiff d = case d of
  Ins n d -> putStrLn ("Ins " ++ string n) >> dumpSFDiff d
  Cpy n d -> putStrLn ("Cpy " ++ string n) >> dumpSFDiff d
  Del n d -> putStrLn ("Del " ++ string n) >> dumpSFDiff d
  _ -> return ()

x .> f = f x
infixl 8 .>

movement1 :: Maybe (TreePos Full NodeType)
movement1 =
  t02 .> toR .> fromTree -- Group 0
  .> firstChild          -- Group 2
  >>= firstChild         -- Group 20
  >>= firstChild         -- Synth 2000
  >>= next               -- Synth 2001
  >>= next               -- Synth 2002
  >>= firstChild         -- Par ["amp":=0.3,"freq":=1320.0,"out":=0.0]

movement2 :: Maybe (TreePos Full NodeType)
movement2 =
  movement1      -- Par ["amp":=0.3,"freq":=1320.0,"out":=0.0])
  >>= parent     -- Synth 2002
  >>= prev       -- Synth 2001
  >>= prev       -- Synth 2000
  >>= firstChild -- Just (Par ["amp":=0.3,"freq":=440.0,"out":=0.0])

d2m :: forall a f txs tys.
       EditScriptL f txs tys -> [TreePos Full a -> Maybe (TreePos Full a)]
d2m d = case d of
  _ -> []

st1 :: Int -> Writer [(Int,Bool)] ()
st1 k | k == 0 = return ()
      | even k = tell [(k,True)] >> st1 (pred k)
      | odd k  = tell [(k,False)] >> st1 (pred k)