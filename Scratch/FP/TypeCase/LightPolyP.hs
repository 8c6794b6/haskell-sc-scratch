{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

-}
module LightPolyP where

data Empty p r = Empty
data Plus g h p r = Inl (g p r) | Inr (h p r)
data Prod g h p r = Prod (g p r) (h p r)
newtype Par p r = Par {unPar :: p}
newtype Rec p r = Rec {unRec :: r}
newtype Comp d h p r = Comp {unComp :: d (h p r)}
newtype Const t p r = Const {unConst :: t}

data Iso a b = Iso {inn :: a->b, out :: b -> a}

listIso = Iso inL outL where
  inL (Inl Empty) = []
  inL (Inr (Prod (Par x) (Rec xs))) = x:xs
  outL [] = Inl Empty
  outL (x:xs) = (Inr (Prod (Par x) (Rec xs)))

class Generic f where
  empty :: f Empty
  plus :: (Rep g,Rep h) => f (Plus g h)
  prod :: (Rep g,Rep h) => f (Prod g h)
  par :: f Par
  rec :: f Rec
  comp :: (Functor d,Rep h) => f (Comp d h)
  constant :: f (Const t)

class Rep g where
  rep :: Generic f => f g

instance Rep Empty where
  rep = empty

instance (Rep g,Rep h) => Rep (Plus g h) where
  rep = plus

instance (Rep g,Rep h) => Rep (Prod g h) where
  rep = prod

instance Rep Par where
  rep = par

instance Rep Rec where
  rep = rec

instance (Functor d,Rep h) => Rep (Comp d h) where
  rep = comp

instance Rep (Const t) where
  rep = constant

------------------------------------------------------------------------------
-- Like GM, adding generic function is declaring a record with single field,
-- a function of the appropriate type.

newtype FMap2 a b c d f =
  FMap2 {appFMap2 :: (a->c) -> (b->d) -> f a b -> f c d}

instance Generic (FMap2 a b c d) where
  empty = FMap2 $ \_ _ _ -> Empty
  plus = FMap2 $ \f g t -> case t of
    Inl x -> Inl (fmap2 f g x)
    Inr y -> Inr (fmap2 f g y)
  prod = FMap2 $ \f g (Prod x y) -> Prod (fmap2 f g x) (fmap2 f g y)
  par = FMap2 $ \f _ (Par t) -> Par (f t)
  rec = FMap2 $ \_ g (Rec t) -> Rec (g t)
  comp = FMap2 $ \f g (Comp t) -> Comp (fmap (fmap2 f g) t)
  constant = FMap2 $ \_ _ (Const t) -> Const t

  -- XXX: NG, type mismatch. Need to unwrap from (Const t) and wrap again.
  -- constant = FMap2 $ \_ _ t -> t

fmap2 :: Rep f => (a -> c) -> (b -> d) -> f a b -> f c d
fmap2 = appFMap2 rep

cata :: Rep f => Iso (f c b) b -> (f c d -> d) -> b -> d
cata iso f = f . fmap2 id (cata iso f) . out iso