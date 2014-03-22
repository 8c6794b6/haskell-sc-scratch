{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Syntax of patterns for serialization and deserialization, take 3.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.E2 where

import Data.Data
import Sound.SC3
import Sound.SC3.Lepton.Pattern.Expression

data E s
  = Leaf String
  | Node String [E s]
  | NodeI String (E Int) [E s]
    deriving (Eq, Show, Data, Typeable)

toE :: E s -> E s
toE = id

instance Functor E where
  fmap f (Leaf s) = Leaf s
  fmap f (Node s es) = Node s (map (fmap f) es)

instance Pval E where
  pval x = Leaf (show x)

instance Plist E where
  plist xs = Node "plist" (map (Leaf . show) xs)

instance Pappend E where
  pappend p1 p2 = Node "pappend" [p1,p2]

instance Pconcat E where
  pconcat ps = Node "pconcat" ps

instance Pseq E where
  pseq pn ps = NodeI "pseq" pn ps