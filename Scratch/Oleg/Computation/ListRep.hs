{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

-}
module ListRep where

------------------------------------------------------------------------------
-- Considering list with standard Church encoding of pairs.

cons x y p = p x y
car p = p true
cdr p = p false

true x y = x 
false x y = y

nil = cons 0 0
isnil l = car l == 0


-- Build simple list.
l1 = cons 1 (cons 2 (cons 3 nil))

-- First element of list
t10 = car l1

-- Second element of list
cadr = car . cdr
t11 = cadr l1

-- But, cannot write a function to get n-th element of a list.
-- Since l and (cdr l) has different type, get an error with below:
-- 
-- > nth 0 l = car l
-- > nth n l = nth (n-1) (cdr l)

------------------------------------------------------------------------------
-- Using type class

-- Making same operations apply to (cdr lst)
-- Operations in questions are `car', `cdr', and `null',
-- which represented by one operation, `dons', deconstructor.
--
class L l where
  cdons :: l a -> (forall l'. L l' => a -> l' a -> w) -> w -> w
  
data Nil a = Nil deriving Show
data Cons t a = Cons a (t a) deriving Show

instance L Nil where
  cdons _ _ nil = nil
  
instance L l => L (Cons l) where
  cdons (Cons h t) cns _ = cns h t
  
l2 :: Cons (Cons (Cons Nil)) Int
l2 = Cons 1 (Cons 2 (Cons 3 Nil))

cnth :: L l => Int -> l a -> a
cnth 0 l = cdons l (\h _ -> h) (error "Empty list")
cnth n l = cdons l (\h t -> (cnth (n-1) t)) (error "Empty list")
  

-- Quantifying over 't' in `Cons a (t a)'. 

class TL l where
  tdons :: l a -> (forall l'. TL l' => a -> l' a -> w) -> w -> w
  
data DL a where
  TNil  :: DL a
  -- TCons :: (TL l, Show (l a)) => a -> (l a) -> DL a
  TCons :: (TL l, L l, Show (l a)) => a -> (l a) -> DL a
  
instance Show a => Show (DL a) where  
  show TNil = "TNil"
  show (TCons a as) = "TCons " ++ show a ++ " (" ++ show as ++ ")"
  
instance TL DL where
  tdons TNil _ nil = nil
  tdons (TCons a as) cns _ = cns a as
  
instance L DL where  
  cdons TNil _ nil = nil
  cdons (TCons a as) cns _ = cns a as

l3 :: DL Int
l3 = TCons 1 (TCons 2 (TCons 3 TNil))

tnth :: TL l => Int -> l a -> a
tnth 0 l = tdons l (\h _ -> h) (error "Empty list")
tnth n l = tdons l (\_ t -> tnth (n-1) t) (error "Empty list")

-- Quantified encoding supports infinite lists

l3i :: DL Int
l3i = TCons 1 (TCons 2 (TCons 3 l3i))

l3i_52 = cnth 45 l3i

------------------------------------------------------------------------------
-- Unwrapping type class with manual type dictionary

dict'nil = ((), \Nil _ nil -> nil)
dict'cons = \dict -> ((), \(Cons h t) cns _ -> cns dict h t)

dons'm (_,x) = x

-- dictionary for the list l2
l2'd = dict'cons $ dict'cons $ dict'cons $ dict'nil

