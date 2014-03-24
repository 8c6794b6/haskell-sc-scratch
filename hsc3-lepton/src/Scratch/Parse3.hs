{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns, take 3.
-}
module Scratch.Parse3 where

import Data.Dynamic

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.Interpreter.Bz
import Sound.SC3.Lepton.Pattern.Interpreter.R

data Exp s
  = Leaf s
  | Node String [Exp s]
  deriving (Eq, Read, Show, Typeable)

unLeaf (Leaf s) = s
unLeaf (Node _ _) = error "Not an atom."

------------------------------------------------------------------------------
-- First try, without type checker.

-- data Term1 repr = forall a. (Show a, Show (repr a)) => Term1 (repr a)

data Term1 repr where
  Term1 :: (Show a, Show (repr a)) => repr a -> Term1 repr

instance (Show a, Show (repr a)) => Show (Term1 repr) where
  show (Term1 a) = show a

-- tc :: (Pval p, Plist p, Show (p Double)) => Exp -> Term p
tc1 e = case e of
  Node "pval" [Leaf x] -> Term1 (pval x)
  Node "plist" as      -> Term1 (plist (map (\x -> unLeaf x) as))

  -- XXX: This does not work.
  -- GHC cannnot deduce (a1 ~ a), cannot deduce q1 and q2 having equal type.
  --
  -- Node "pappend" [p1,p2] ->
  --   case tc1 p1 of Term q1 -> case tc1 p2 of Term q2 -> Term (pappend q1 q2)

------------------------------------------------------------------------------
-- Second try, with dynamic type.

data Term2 r where
  Term2 :: forall (r :: * -> *) a. (Show a, Typeable a)
           => TypeRep -> r a -> Term2 r

t2_show_bz :: Term2 Bz -> String
t2_show_bz (Term2 _ bz) = show bz

t2_print_r :: Term2 R -> IO ()
t2_print_r (Term2 _ r) = mapPIO_ print r

-- e01, e02 :: Exp Double
e01 = Node "pval" [Leaf 1]
e01' = Node "pval" [Leaf 2]
e02 = Node "plist" [Leaf 2, Leaf 3, Leaf 4]
e03 = Node "pappend" [e01,e02]
e04 = Node "pappend" [e01,e01']

-- tc2 :: forall (r :: * -> *) a.
--        (Pval r, Plist r, Pconcat r, Pappend r, Typeable a, Show a)
--        => Exp a -> Either String (Term2 r)

tc2 e = case e of
  Node "pval" [Leaf x] -> return $ Term2 (typeOf x) (pval x)
  Node "plist" (as@(a:_)) -> return $ Term2 (typeOf a) (plist (map unLeaf as))

  -- XXX: This does not work.
  -- Again we get `Could not deduce (a2 ~ a1)` error.
  --

  Node "pappend" [p1,p2] ->
    case p2 `asTypeOf` p1 of
      p2' -> do
        Term2 ty1 e1 <- tc2 p1
        Term2 ty2 e2 <- tc2 p2'
        if True -- ty1 == ty2
          then return $ Term2 ty2 (pappend e2 e2)
          -- then return $ Term2 (typeOf (pappend e1 e2)) (pappend e1 e2)
          else error "type mismatch"

------------------------------------------------------------------------------
-- Try implementing TypeCheck with Data.Typeable.

------------------------------------------------------------------------------
-- Try implementing serialization/deserialization without de bruijn (TTF.hs).

------------------------------------------------------------------------------
-- Third try, with type checking environment

type VarName = String
data VarDesc = VarDesc TypeRep VarName

class Var g h where
  findvar :: (Pval p, Plist p, Pappend p)
    => VarName -> g -> Either String ((Term2 p),h)

instance Var () () where
  findvar name _ = fail $ "Unbound variable: " ++ name


-- Not working.
--
-- instance Var g h => Var (VarDesc, g) (t,h) where
--   findvar name (VarDesc t name',_)
--      | name == name' = return $ (Term2 (typeOf t) t,typeOf t)
