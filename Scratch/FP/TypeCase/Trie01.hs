{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

Trie with smart data type.

-}
module Trie01 where

import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import LIGD (Unit(..), Sum(..), Prod(..))
import LightPolyP (Iso(..))

class FMap g where
  unit :: g Unit v Maybe
  plus :: g a v c -> g b v d -> g (Sum a b) v (PlusCase c d)
  prod :: g a (d v) c -> g b v d -> g (Prod a b) v (ProdCase c d)
  datatype :: g a v c -> Iso b a -> Iso (d v) (c v) -> g b v d
  int :: g Int v IntMap

data OptPair a b = Null | Pair a b

newtype PlusCase a b v = PlusCase {unPlus :: OptPair (a v) (b v)}
newtype ProdCase a b v = ProdCase {unProd :: a (b v)}

newtype FList c v =
  FList {unFList :: (PlusCase Maybe (ProdCase c (FList c))) v}

list :: FMap g => g a (FList c v) c -> g [a] v (FList c)
list ra = datatype (plus unit (prod ra (list ra))) listEP
          (Iso unFList FList)

listEP :: Iso [a] (Sum Unit (Prod a [a]))
listEP = Iso fromL toL where
  fromL x = case x of
    [] -> Inl Unit
    (y:ys) -> Inr (Prod y ys)
  toL x = case x of
    Inl Unit -> []
    Inr (Prod x xs) -> x:xs

newtype EmptyTrie a v t = EmptyTrie {empty :: t v}

instance FMap EmptyTrie where
  unit = EmptyTrie Nothing
  int = EmptyTrie (IM.empty)
  plus ra rb = EmptyTrie (PlusCase Null)
  prod ra rb = EmptyTrie (ProdCase (empty ra))
  datatype ra iso iso2 = EmptyTrie (out iso2 (empty ra))

newtype LUp a v t = LUp {lookUp :: a -> t v -> Maybe v}

instance FMap LUp where
  unit = LUp $ \_ fm -> fm
  int = LUp $ \i fm -> IM.lookup i fm
  plus ra rb = LUp $ \t fm ->
    case unPlus fm of
      Null -> Nothing
      Pair fma fmb -> case t of
        Inl l -> lookUp ra l fma
        Inr r -> lookUp rb r fmb
  prod ra rb = LUp $ \(Prod x y) (ProdCase fma) ->
    (lookUp ra x >=> lookUp rb y) fma
  datatype ra iso iso2 =
    LUp $ \t r -> lookUp ra (inn iso t) (inn iso2 r)

{-

ghci> let t = list int in lookUp t [1,2,3] (empty t)
Nothing

ghci> lookUp int 3 (IM.fromList $ zip [1..10] "lookup with FMap")
Just 'o'

-}

data Single a v t = Single
  { emptyT :: EmptyTrie a v t
  , single :: a -> v -> t v }

instance FMap Single where
  unit = Single unit $ \_ v -> Just v
  int = Single int $ \i v -> IM.singleton i v
  plus ra rb = Single (plus (emptyT ra) (emptyT rb)) $ \i v ->
    case i of
      Inl l -> PlusCase (Pair (single ra l v) (empty (emptyT rb)))
      Inr r -> PlusCase (Pair (empty (emptyT ra)) (single rb r v))
  prod ra rb = Single (prod (emptyT ra) (emptyT rb)) $ \i v ->
    case i of
      Prod x y -> ProdCase (single ra x (single rb y v))
  datatype ra iso iso2 = Single (datatype (emptyT ra) iso iso2) $ \i v ->
    out iso2 (single ra (inn iso i) v)

{-

ghci> lookUp int 1 (single int 1 "blah")
Just "blah"

ghci> lookUp int 2 (single int 1 "blah")
Nothing

ghci> lookUp (list int) [1,2,3] (single (list int) [1,2,3] "foo")
Just "foo"

-}