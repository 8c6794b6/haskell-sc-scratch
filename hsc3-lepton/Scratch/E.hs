{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

E, for serializing/deserializing expression tree.
-}
module Scratch.E where

import Scratch.Etree

-- | Newtype wrapper for converting to expression tree.
newtype E h a = E {unE :: Int -> Etree}

toE :: E h a -> E h a
toE = id

etree :: E h a -> Etree
etree e = unE e 0

instance Show (E h a) where
  show e = show $ etree e

instance Eq (E h a) where
  e1 == e2 = show e1 == show e2

instance Ord (E h a) where
  compare _ _ = EQ
