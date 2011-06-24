{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From: /species-pearl.pdf/.
--
module Pearl.Species where

import qualified Data.List as L

class Enumerable f where
  enumerate :: [a] -> [f a]

data Zero a

instance Functor Zero where
  fmap = undefined

instance Enumerable Zero where
  enumerate _ = []

data One a = One

instance Functor One where
  fmap _ One = One

instance Enumerable One where
  enumerate [] = [One]
  enumerate _  = []

data X a = X a

instance Functor X where
  fmap f (X a) = X (f a)

instance Enumerable X where
  enumerate [x] = [X x]
  enumerate _   = []

data (f :+ g) a = Inl (f a) | Inr (g a)

infixl 6 :+

instance (Functor f, Functor g) => Functor (f :+ g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Enumerable f, Enumerable g) => Enumerable (f :+ g) where
  enumerate ls = map Inl (enumerate ls) ++ map Inr (enumerate ls)

(+:) :: (f a -> g a) -> (h a -> j a) -> (f :+ h) a -> (g :+ j) a
(fg +: hj) (Inl fa) = Inl (fg fa)
(fg +: hj) (Inr ha) = Inr (hj ha)

infix 1 :<->
data (f :<-> g) = (:<->) { to :: forall a. f a -> g a
                         , from :: forall a. g a -> f a }
ident :: f :<-> f
ident = id :<-> id

(>>>) :: (f :<-> g) -> (g :<-> h) -> (f :<-> h)
(fg :<-> gf) >>> (gh :<-> hg) = (gh . fg) :<-> (gf . hg)

inv :: (f :<-> g) -> (g :<-> f)
inv (fg :<-> gf) = gf :<-> fg

sumIdL :: Zero :+ f :<-> f
sumIdL = (\(Inr x) -> x) :<-> Inr

sumComm :: f :+ g :<-> g :+ f
sumComm = swapSum :<-> swapSum where
  swapSum (Inl x) = Inr x
  swapSum (Inr x) = Inl x

sumAssoc :: f :+ (g :+ h) :<-> (f :+ g) :+ h
sumAssoc = reAssocL :<-> reAssocR where
  reAssocL (Inl x)       = Inl (Inl x)
  reAssocL (Inr (Inl x)) = Inl (Inr x)
  reAssocL (Inr (Inr x)) = Inr x
  reAssocR (Inl (Inl x)) = Inl x
  reAssocR (Inl (Inr x)) = Inr (Inl x)
  reAssocR (Inr x)       = Inr (Inr x)

inSumL :: (f :<-> g) -> (f :+ h :<-> g :+ h)
inSumL ~(fg :<-> gf) = (fg +: id) :<-> (gf +: id)

inSumR :: (f :<-> g) -> (h :+ f :<-> h :+ g)
inSumR ~(fg :<-> gf) = (id +: fg) :<-> (id +: gf)

data (f :* g) a = f a :* g a
infixl 7 :*

instance (Functor f, Functor g) => Functor (f :* g) where
  fmap h (x :* y) = fmap h x :* fmap h y

instance (Enumerable f, Enumerable g) => Enumerable (f :* g) where
  enumerate ls = [ x :* y | (fls, gls) <- splits ls
                           , x <- enumerate fls
                           , y <- enumerate gls ]

splits :: [a] -> [([a], [a])]
splits [] = [([],[])]
splits (x:xs) = (map . first) (x:) ss ++ (map . second) (x:) ss where
  ss = splits xs
  first f (x,y) = (f x, y)
  second f (x,y) = (x, f y)

prodIdL :: (One :* f) :<-> f
prodIdL = (\(One :* x) -> x) :<-> (One :*)

prodIdR :: (f :* One) :<-> f
prodIdR = (\(x :* One) -> x) :<-> (:* One)

prodAbsorbL :: (Zero :* f) :<-> Zero
prodAbsorbL = (\(f :* _) -> f) :<-> (\g -> g :* undefined)

prodComm :: f :* g :<-> g :* f
prodComm = swapProd :<-> swapProd where
  swapProd (f :* g) = g :* f

prodAssoc :: f :* (g :* h) :<-> (f :* g) :* h
prodAssoc = fr :<-> fl where
  fr (f :* (g :* h)) = (f :* g) :* h
  fl ((f :* g) :* h) = f :* (g :* h)

prodDistrib :: f :* (g :+ h) :<-> (f :* g) :+ (f :* h)
prodDistrib = fr :<-> fl where
  fr (f :* (Inl x)) = Inl (f :* x)
  fr (f :* (Inr x)) = Inr (f :* x)
  fl (Inl (f :* x)) = f :* (Inl x)
  fl (Inr (f :* x)) = f :* (Inr x)

inProdL :: (f :<-> g) -> (f :* h :<-> g :* h)
inProdL ~(f :<-> g) = undefined

inProdR :: (f :<-> g) -> (h :* f :<-> h :* g)
inProdR ~(f :<-> g) = undefined

instance Enumerable [] where
  enumerate = L.permutations

listRec :: [] :<-> One :+ (X :* [])
listRec = unroll :<-> roll where
  unroll []     = Inl One
  unroll (x:xs) = Inr (X x :* xs)
  roll (Inl One)         = []
  roll (Inr (X x :* xs)) = x : xs

data BTree a
  = Empty
  | Node a (BTree a) (BTree a)

data Paren a
  = Leaf a
  | Pair (Paren a) (Paren a)

bTreeRec :: BTree :<-> One :+ X :* BTree :* BTree
bTreeRec = unroll :<-> roll where
  unroll Empty        = Inl One
  unroll (Node x l r) = Inr (X x :* l :* r)
  roll (Inl One)             = Empty
  roll (Inr (X x :* l :* r)) = Node x l r

parenRec :: Paren :<-> X :+ Paren :* Paren
parenRec = unroll :<-> roll where
  unroll (Leaf x) = Inl (X x)
  unroll (Pair l r) = Inr (l :* r)
  roll (Inl (X x)) = Leaf x
  roll (Inr (l :* r)) = Pair l r

bToP :: X :* BTree :<-> Paren
bToP = inProdR bTreeRec >>>
       prodDistrib >>>
       inSumL prodIdR >>>
       inSumR (inProdR
               (inProdL prodComm >>>
                inv prodAssoc) >>>
               prodAssoc >>>
               inProdL bToP >>>
               inProdR bToP) >>>
       inv parenRec

newtype (f :. g) a = C {unC :: f (g a)}

instance (Functor f, Functor g) => Functor (f :. g) where
  fmap h = C . (fmap . fmap) h . unC

instance (Enumerable f, Enumerable g) => Enumerable (f :. g) where
  enumerate ls =
    [C y | p <- partitions ls
         , gs <- mapM enumerate p
         , y <- enumerate gs]

partitions :: [a] -> [[[a]]]
partitions []     = [[]]
partitions (x:xs) = [(x:ys):p | (ys,zs) <- splits xs
                              , p <- partitions zs]
