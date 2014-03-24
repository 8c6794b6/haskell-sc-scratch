{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Type representation used in pattern expression tree deserializer.
-}
module Scratch.Type01 where

data a :<-> b = EP {from :: a->b, to :: b->a}

data Sum a b = Inl a | Inr b
data Prod a b = Prod a b
data Unit = Unit

data Rep t
  = RUnit (t :<-> Unit)
  | RInt  (t :<-> Int)
  | RChar (t :<-> Char)
  | forall a b. RSum (Rep a) (Rep b) (t :<-> Sum a b)
  | forall a b. RProd (Rep a) (Rep b) (t :<-> Prod a b)

self :: a :<-> a
self = EP {from=id, to=id}

rUnit :: Rep Unit
rUnit = RUnit self

rInt :: Rep Int
rInt = RInt self

rChar :: Rep Char
rChar = RChar self

rSum :: Rep a -> Rep b -> Rep (Sum a b)
rSum a b = RSum a b self

rProd :: Rep a -> Rep b -> Rep (Prod a b)
rProd a b = RProd a b self

rList :: forall a. Rep a -> Rep [a]
rList ra = RSum rUnit (rProd ra (rList ra)) (EP from to) where
  from [] = Inl Unit
  from (x:xs) = Inr (Prod x xs)
  to (Inl Unit) = []
  to (Inr (Prod x xs)) = x:xs

eq :: forall t. Rep t -> t -> t -> Bool
eq (RInt ep) t1 t2 = from ep t1 == from ep t2
eq (RChar ep) t1 t2 = from ep t1 == from ep t2
eq (RUnit ep) _ _ = True
eq (RSum a b ep) t1 t2 = case (from ep t1, from ep t2) of
  (Inl x, Inl y) -> eq a x y
  (Inr x, Inr y) -> eq b x y
  _ -> False
eq (RProd a b ep) t1 t2 = case (from ep t1, from ep t2) of
  (Prod x y, Prod x' y') -> eq a x x' && eq b y y'

class TypeRep t where
  rep :: Rep t

instance TypeRep Unit where
  rep = rUnit

instance TypeRep Int where
  rep = rInt

instance TypeRep Char where
  rep = rChar

instance (TypeRep a, TypeRep b) => TypeRep (Sum a b) where
  rep = rSum rep rep

instance (TypeRep a, TypeRep b) => TypeRep (Prod a b) where
  rep = rProd rep rep

instance TypeRep a => TypeRep [a] where
  rep = rList rep

ceq :: forall t. TypeRep t => t -> t -> Bool
ceq a b = eq rep a b