{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Parsing patterns, take 9.
-}
module Scratch.Parse9 where

import Control.Monad
import System.Random (Random)

import Data.Binary
import Sound.SC3

import Sound.SC3.Lepton.Pattern.Dummy
import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Play

import Scratch.EInstance2 ()
import Scratch.Etree (Etree(..),ppTree)
import Scratch.L
import Scratch.LInstance2 ()
import Scratch.PC02
import Scratch.Parse5 (E(..), etree, toE)
import Scratch.S
import Scratch.SInstance2 ()
import Scratch.Type00

data Term r h where
  Term :: TR t -> r h t -> Term r h

class VarEnv g h | g -> h where
  findvar :: Plambda r => Int -> g -> Either String (Term r h)

data VarDesc t where
  VarDesc :: forall t. Int -> TR t -> VarDesc t

instance VarEnv () () where
  findvar _ _ = Left "Variable unbound"

instance (VarEnv g h) => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

type FromTree r =
  forall g h.
  ( Pint r, Pdouble r, Pappend r, Pconcat r
  , Preplicate r, Pseq r, Pforever r,Pcycle r
  , Prand r, Pshuffle r, Ptuple r
  , Plambda r, VarEnv g h
  , Psnew r, Pmerge r, Ppar r
  ) => (Etree,g) -> Either String (Term r h)

fromTreeE :: forall r. FromTree r -> FromTree r
fromTreeE self (e,g) = case e of
  Node "pdouble" [Leaf x] -> return $ Term (TR tdouble) (pdouble $ decode x)
  Node "pappend" [e1,e2] -> do
    Term (TR t1) v1 <- self (e1,g)
    Term (TR t2) v2 <- self (e2,g)
    case cmpTy t1 t2 of
      Just Equal -> return $ Term (TR t1) (pappend v1 v2)
  Node "preplicate" [e1,e2] -> do
    Term (TR t1) v1 <- self (e1,g)
    Term (TR t2) v2 <- self (e2,g)
    case cmpTy t1 TyInt of
      Just Equal -> return $ Term (TR t2) (preplicate v1 v2)
  Node "var" [Leaf x] -> findvar (decode x) g
  Node "plam" [Leaf v,ty,body] -> do
    let v' = decode v
    ExtTR (TR argty) <- treeToTR ty
    Term (TR bty) bval <- self (body,(VarDesc v' argty,g))
    return $ Term (TR (tarr argty (tlist bty))) (plam argty bval)
  Node "papp" [e1,e2] -> do
    Term (TR t1) v1 <- self (e1,g)
    Term (TR t2) v2 <- self (e2,g)
    case toTy t1 of
      TyArr bndty (TyList bodyty)-> return $ Term (TR $ toTy bodyty) (papp v1 v2)
    -- case appTy t1 of
    --   (bndty,bodyty) -> case cmpTy bndty t2 of
    --     Just Equal -> return $ Term (TR $ toTy bodyty) (papp v1 v2)


treeToTR :: Etree -> Either String ExtTR
treeToTR t = case t of
  Node "TyInt" [] -> return $ ExtTR tint
  Node "TyDouble" [] -> return $ ExtTR tdouble
  Node "TyList" [e] -> do
    ExtTR (TR t) <- treeToTR e
    return $ ExtTR $ tlist t
  Node "TyToOSC" [e] -> do
    ExtTR (TR t) <- treeToTR e
    return $ ExtTR $ ttoosc t
  Node "TyTup" [e1,e2] -> do
    ExtTR (TR t1) <- treeToTR e1
    ExtTR (TR t2) <- treeToTR e2
    return $ ExtTR $ ttup t1 t2
  Node "TyArr" [e1,e2] -> do
    ExtTR (TR t1) <- treeToTR e1
    ExtTR (TR t2) <- treeToTR e2
    return $ ExtTR $ tarr t1 t2

------------------------------------------------------------------------------
-- Sample terms

p01 = plam tint pz