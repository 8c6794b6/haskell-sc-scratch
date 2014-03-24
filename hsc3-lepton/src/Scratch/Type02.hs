{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/Haskell/typeEQ.html>

-}
module Scratch.Type02 where

import Scratch.Type00

data HTrue
data HFalse

instance Show HTrue where show _ = "HTrue"
instance Show HFalse where show _ = "HFalse"

class TypeEq x y b | x y -> b where
  teq :: x -> y -> b
  teq = undefined

instance TypeEq x x HTrue
instance b ~ HFalse => TypeEq x y b

type family HBool a :: *
type instance HBool HFalse = Bool
type instance HBool HTrue = Bool

{-
ghci> undefined :: TypeEq Int Char r => r
HFalse

ghci> undefined :: TypeEq Int Int r => r
HTrue

ghci> undefined :: TypeEq Int (Maybe (Either [Int] (Char,String,Bool))) r => r
HFalse
-}

eqTy :: (Show r, TypeEq x y r) => x -> y -> Bool
eqTy x y= case show (teq x y) of
  "HTrue" -> True
  "HFalse" -> False
