{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Scratch written while reading GADT page in wikibook, from 
-- <http://en.wikibooks.org/wiki/Haskell/GADT> .
-- 
module Wikibook where

-- 
-- Expression example
-- 

data Expr a where
  I   :: Int -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Expr Int -> Expr Int -> Expr Bool

eval :: Expr a -> a 
eval (I x) = x
eval (B x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2) = eval e1 == eval e2

-- 
-- Example of non-haskell98 data type.
--
-- FooInGadtClothing has haskell98 equivalent, while TrueGadtFoo has not.
--

data FooInGadtClothing a where
  MkFooInGadtClothing :: a -> FooInGadtClothing a
  
-- It is impossible to express this data type in haskell98.
data TrueGadtFoo a where  
  MkTrueGadtFoo :: a -> TrueGadtFoo Int
  
--  
-- Example of safe head.  
--  
  
data Empty
data NonEmpty

data SafeList a b where
  SNil  :: SafeList a Empty 
  SCons :: a -> SafeList a b -> SafeList a NonEmpty
  
safeHead :: SafeList a NonEmpty -> a
safeHead (SCons a _) = a

data NotSafe
data Safe a 

data MarkedList :: * -> * -> * where
  MNil  :: MarkedList t NotSafe
  MCons :: a -> MarkedList a b -> MarkedList a (Safe b)
  
instance Show a => Show (MarkedList a b) where
  show MNil = "MNil"
  show (MCons a cs) = "MCons " ++ show a ++ " (" ++ show cs ++ ")"
  
safeHead' :: MarkedList a (Safe a) -> a
safeHead' (MCons a _) = a

safeTail' :: MarkedList a (Safe b) -> MarkedList a b
safeTail' (MCons _ cs) = cs

silly 0 = MNil

-- Below not working.

-- silly 1 = MCons () MNil
-- silly n = MCons (silly (n-1))