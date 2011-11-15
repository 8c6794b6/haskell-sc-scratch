{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

-}
module PFDS.Scratch01 where

import Prelude hiding (drop, reverse, take, (++))
import qualified Prelude

data Stream a = Nil | Cons a (Stream a)

enum_stream :: (Eq x, Enum x) => x -> x -> Stream x
enum_stream to from
  | to == from = Cons to Nil
  | otherwise  = Cons to (enum_stream (succ to) from)

instance Show a => Show (Stream a) where
  show s0 = case s0 of
    Nil       -> "{}"
    Cons x s1 -> '{' : show x Prelude.++ go s1 where
      go t0 = case t0 of
        Nil       -> "}"
        Cons y t1 -> ',' : show y Prelude.++ go t1

take :: Int -> Stream a -> Stream a
take n l = case (n,l) of
  (0, _) -> Nil
  (_, Nil) -> Nil
  (_, Cons x xs) -> Cons x (take (n-1) xs)

(++) :: Stream a -> Stream a -> Stream a
s ++ t = case s of
  Nil       -> t
  Cons x xs -> Cons x (xs ++ t)

drop :: Int -> Stream a -> Stream a
drop n l = case (n,l) of
  (0, _)   -> l
  (_, Nil) -> Nil
  (_, Cons x xs) -> drop (n-1) xs

drop2 :: Int -> Stream a -> Stream a
drop2 n l =
  let drop2' 0 l' = l'
      drop2' _ Nil = Nil
      drop2' m (Cons x xs) = drop2' (m-1) xs
  in  drop2' n l

reverse :: Stream a -> Stream a
reverse ns =
  let f xs ys = case xs of
        Nil       -> ys
        Cons z zs -> f zs (Cons z ys)
  in  f ns Nil


