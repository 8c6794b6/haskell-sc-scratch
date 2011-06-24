{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Pearl.Nameplate where

import Control.Monad.State
import Data.Data
import Data.Generics.Uniplate.Data

data Name = Name String (Maybe Int)
 deriving (Eq, Show)

class Nom a where
  (==@) :: a -> a -> Bool

data a :\\ b = a :\\ b

instance (Eq a, Eq b) => Eq (a :\\ b) where
  (a :\\ b) == (c :\\ d) = undefined

class Monad m => FreshM m where
  renameFM :: Name -> m Name

class Subst t u where
  (|->) :: FreshM m => Name -> t -> m u

class FreeVars t u where
  fvars :: t -> u -> [Name]

data Tree = Leaf Nid
          | Branch Nid [Tree]
          deriving (Eq, Show, Data, Typeable)

data Nid = Known Int
         | Anon
         deriving (Eq, Show, Data, Typeable)

t1 :: Tree
t1 = Branch (Known 0)
     [ Leaf Anon
     , Branch (Known 2)
        [ Leaf Anon
        , Branch Anon
          [Leaf Anon
          ,Leaf Anon]
        , Branch Anon []
        , Leaf (Known 22)]
     , Leaf (Known 3)]

fillIds :: Tree -> Int -> Tree
fillIds t n = fillIt (maximum (n:ids)) t where
  ids = [n | Leaf (Known n) <- universe t] ++
        [n | Branch (Known n) _ <- universe t]

fillIt :: Int -> Tree -> Tree
fillIt n (Leaf Anon)      = Leaf (Known (n+1))
fillIt n (Leaf x)            = Leaf x
fillIt n (Branch Anon ts) = Branch (Known (n+1)) (map (fillIt (n+1)) ts)
fillIt n (Branch x ts)       = Branch x (map (fillIt (n+1)) ts)
