{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* TypeCase: A Design Pattern for Type-Indexed Functions

LIGD stands for /Lightweight Implementation of Generics and Dynamics/.

-}
module LIGD where

data Sum a b = Inl a | Inr b deriving Show
data Prod a b = Prod a b deriving Show
data Unit = Unit deriving Show

data a :<-> b = EP {from :: a -> b, to :: b -> a}

data Rep t
  = RUnit (t :<-> Unit)
  | RInt (t :<-> Int)
  | RChar (t :<-> Char)
  | ∀ a b. RSum (Rep a) (Rep b) (t :<-> (Sum a b))
  | ∀ a b. RProd (Rep a) (Rep b) (t :<-> (Prod a b))
    
instance Show (Rep t) where    
  show r = case r of
    RUnit _ -> "RUnit"
    RInt _  -> "RInt"
    RChar _ -> "RChar"
    RSum a b _ -> "RSum (" ++ show a ++ " " ++ show b ++ ")"
    RProd a b _ -> "RProd (" ++ show a ++ " " ++ show b ++ ")"
    
self :: a :<-> a    
self = EP id id

rUnit :: Rep Unit
rUnit = RUnit self

rInt :: Rep Int
rInt = RInt self

rChar :: Rep Char
rChar = RChar self

rSum :: Rep a -> Rep b -> Rep (Sum a b)
rSum ra rb = RSum ra rb self

rProd :: Rep a -> Rep b -> Rep (Prod a b)
rProd ra rb = RProd ra rb self

rList :: Rep a -> Rep [a]
rList ra = RSum rUnit (rProd ra (rList ra)) (EP f t) where
  f bs = case bs of
    []     -> Inl Unit
    (x:xs) -> Inr (Prod x xs)
  t x = case x of
    Inl Unit        -> []
    Inr (Prod x xs) -> x:xs
    
eq :: ∀t. Rep t -> t -> t -> Bool
eq t t1 t2 = case t of
  RInt ep -> from ep t1 == from ep t2
  RChar ep -> from ep t1 == from ep t2
  RUnit _  -> True
  RSum ra rb ep -> case (from ep t1, from ep t2) of
    (Inl x, Inl y) -> eq ra x y
    (Inr x, Inr y) -> eq rb x y
    _              -> False
  RProd ra rb ep -> case (from ep t1, from ep t2) of
    (Prod x y, Prod x' y') -> eq ra x x' && eq rb y y'

class TypeRep t where
  rep :: Rep t
  
instance TypeRep Unit where  
  rep = rUnit
  
instance TypeRep Int where
  rep = rInt
  
instance TypeRep Char where  
  rep = rChar

instance (TypeRep a,TypeRep b) => TypeRep (Sum a b) where
  rep = rSum rep rep

instance (TypeRep a,TypeRep b) => TypeRep (Prod a b) where
  rep = rProd rep rep

instance TypeRep a => TypeRep [a] where
  rep = rList rep
  
instance TypeRep () where  
  rep = RUnit (EP f t) where
    f () = Unit
    t Unit = ()
  
instance (TypeRep a,TypeRep b) => TypeRep (a,b) where
  rep = rTup rep rep where
    rTup ra rb = RProd ra rb (EP f t) where
      f (x,y) = Prod x y
      t (Prod x y) = (x,y)
      
instance (TypeRep a,TypeRep b) => TypeRep (Either a b) where
  rep = rEither rep rep where
    rEither ra rb = RSum ra rb (EP f t) where
      f x = case x of
        Right r -> Inr r
        Left  l -> Inl l
      t x = case x of
        Inr r -> Right r
        Inl l -> Left l
        
ceq :: ∀ t. TypeRep t => t -> t -> Bool
ceq a b = eq rep a b

rSize :: TypeRep t => t -> Int
rSize = rSize' rep 

rSize' :: Rep t -> t -> Int
rSize' rt x = case rt of
  RUnit _ -> 0
  RInt _ -> 1
  RChar _ -> 1
  RSum a b e -> case from e x of
    Inl l -> rSize' a l
    Inr r -> rSize' b r
  RProd a b e -> case from e x of
    Prod v1 v2 -> rSize' a v1 + rSize' b v2

