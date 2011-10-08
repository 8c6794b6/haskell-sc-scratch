{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Workaround for dummy instance of Unary and Random class.

Functions are unused but type signatures are used.  When recursion for
value patterns, Int patterns, and ToOSC patterns could be isolated,
these dummy instance definitions could be removed.

Warnings for missing methods are ignored with
'-fno-warn-missing-methods' pragma.

-}
module Sound.SC3.Lepton.Pattern.Dummy () where

import System.Random

import Sound.SC3

import Sound.SC3.Lepton.Pattern.ToOSC

instance Random String where
  random  = undefined
  randomR = undefined

instance Random a => Random (ToOSC a) where
  random = undefined
  randomR = undefined

instance Floating Int
instance Fractional Int
instance UnaryOp Int
