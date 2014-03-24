{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : non-portable (GADTs, RankNTypes, etc)

Parsing patterns, take 6.
-}
module Scratch.Parse6 where

import Debug.Trace

import Sound.SC3.Lepton.Pattern.Expression

import Scratch.Parse5 (Etree(..))
import Scratch.L

import qualified Data.Binary as Bin

newtype E h a = E {unE :: Int -> Etree}

etree :: E h a -> Etree
etree e = unE e 0

instance Pval (E h) where
  pval x = E $ \_ -> Node "pval" [Leaf (Bin.encode x)]

instance Plist (E h) where
  plist xs = E $ \_ -> Node "plist" [Leaf (Bin.encode xs)]

instance Pappend (E h) where
  pappend a b = E $ \h -> Node "pappend" [unE a h,unE b h]

instance Plc E where
  pz = E $ \h -> Node "var" [Leaf $ Bin.encode (pred h)]
  ps v = E $ \h -> unE v h
  lam k = E $ \h ->
    let v = Leaf $ Bin.encode h
        ty = Node "TyDouble" [] -- XXX: constantly TyDouble
        body = unE k (succ h)
    in  Node "lam" [v,ty,body]
  app e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]

data Ty r t where
  TyInt :: Ty r Int
  TyDouble :: Ty r Double
  TyArr :: Ty r a -> Ty r b -> Ty r (a->b)
  TyFun :: Ty r a -> Ty r (r h a)
  TyApp :: Ty r (r h a) -> Ty r a

instance Show (Ty r t) where
  show t = case t of
    TyInt     -> "Int"
    TyDouble  -> "Double"
    TyArr a b -> "(" ++ show a ++ " -> " ++ show b ++ ")"
    _         -> "Other"

data ExtTy r where
  ExtTy :: Ty r t -> ExtTy r

data Equal (f :: * -> * -> *) (g :: * -> * -> *) a b where
  Equal :: forall f c. Equal f f c c

cmpTy :: Ty r a -> Ty r b -> Maybe (Equal r r a b)
cmpTy a b = case (a,b) of
  (TyInt,TyInt) -> Just Equal
  (TyDouble,TyDouble) -> Just Equal
  (TyArr a1 b1, TyArr a2 b2) -> do
    Equal <- cmpTy a1 a2
    Equal <- cmpTy b1 b2
    return Equal
  _ -> Nothing

data Term r h where
  Term :: Ty r t -> r h t -> Term r h

class VarEnv r g h | g -> h where
  findvar :: Plc r => Int -> g -> Either String (Term r h)

data VarDesc r t where
  VarDesc :: Int -> Ty r t -> VarDesc r t

instance VarEnv r () () where
  findvar _ _ = error "Unbound variable"

instance VarEnv r g h => VarEnv r (VarDesc r t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

type FromTree =
  forall r t g h.
  (Plc r, VarEnv r g h)
  => (Etree,Ty r t,g) -> Either String (Term r h)

newtype W h a =
  W {unW :: forall r.
     (Plc r, Pval (r h), Plist (r h), Pappend (r h))
     => r h a}

instance Pval (W r) where pval x = W (pval x)
instance Plist (W r) where plist xs = W (plist xs)
instance Plc W where
  pz = W pz
  ps (W v) = W undefined
  lam = undefined
  app = undefined

-- fixE :: (forall r. FromTree r -> FromTree r) -> FromTree r
fixE :: (FromTree -> FromTree) -> FromTree
fixE f = f (fixE f)

-- XXX: Use wrapper like take04?
-- newtype W h a = W {unW:: forall r h. Pattern (r h) => r h a}

-- fromTree :: forall r. FromTree r -> FromTree r
fromTree :: FromTree -> FromTree
fromTree self (e,t,g) = case e of
  Node "var" [Leaf x] -> let x' = Bin.decode x :: Int in findvar x' g
  Node "lam" [Leaf v,ty,body] -> do
    let v' = Bin.decode v
    ExtTy argty <- tree2ty ty
    Term bodyty body' <- self (body,undefined,((VarDesc v' argty),g))
    return $ Term (TyArr argty (TyFun bodyty)) (lam body')
  -- Node "app" [e1,e2] -> do
  --   Term (TyArr bndty (TyFun resty)) e1' <- self (e1,undefined,g)
  --   Term argty e2' <- self (e2,undefined,g)
  --   case cmpTy bndty argty of
  --     -- Just Equal -> return $ Term resty (app e1' e2')
  --     Just Equal -> return undefined
  -- Node "pval" [Leaf x] -> do
  --   return $ Term TyDouble (pval (Bin.decode x))

tree2ty :: forall r. Etree -> Either String (ExtTy r)
tree2ty e = case e of
  Node "TyDouble" [] -> return $ ExtTy TyDouble
  Node "TyInt" [] -> return $ ExtTy TyInt
  Node "TyArr" [e1,e2] -> do
    ExtTy t1 <- tree2ty e1
    ExtTy t2 <- tree2ty e2
    return $ ExtTy (TyArr t1 t2)