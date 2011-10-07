{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Module to hold Term and VerEnv.
Separated to this module due to use in TemplateHaskell code.

-}
module Scratch.Pattern.Term00 where

import Scratch.Pattern.Type00
import Scratch.Pattern.PC02

-- | Variable environment.
class VarEnv g h | g -> h where
  findvar :: Plambda r => Int -> g -> Either String (Term r h)

-- | Variable description, de bruijn index coupled with its type.
data VarDesc t where
  VarDesc :: Int -> Ty t -> VarDesc t

instance VarEnv () () where
  findvar _ _ = Left "Variable unbound"

instance (VarEnv g h) => VarEnv (VarDesc t,g) (t,h) where
  findvar i (VarDesc j t,g)
    | i == j = return $ Term t pz
    | otherwise = findvar i g >>= \(Term t' v) -> return $ Term t' (ps v)

-- | Quantified wrapper for typed expressions.
data Term r h where
  Term :: Ty t -> r h t -> Term r h
