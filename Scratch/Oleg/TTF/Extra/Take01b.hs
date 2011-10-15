{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

For deserializing expression, take 1, variation b.
Coming back to this Sym class after doing Take04d.

The problem with Sym class defined here is that it does not contain
class method to explicitly denote variable. During the deserialization,
there exists needs for referring variable as variable.

How can we do this without using z and s in class method?

-}
module Take01b where

import Control.Monad.Instances ()

import Type01

class Sym r where
  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  lam :: (r a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b

e01 = add (int 1) (int 2)
e02 = lam (\x -> add x x)
e03 = lam (\x -> add (app x (int 1)) (int 2))
e04 = app e03 (lam (\x -> (add (add x x) x)))
e05 = lam (\x -> lam (\y -> add x y))
e06 = app (app e05 (int 3)) (int 7)

------------------------------------------------------------------------------
-- Evaluate

newtype R a = R {unR :: a}

instance Sym R where
  int x = R x
  add e1 e2 = R $ unR e1 + unR e2
  lam f = R $ \x -> unR $ f (R x)
  app e1 e2 = R $ (unR e1) (unR e2)

eval :: R a -> a
eval = unR

------------------------------------------------------------------------------
-- Valid haskell syntax string representation

newtype S a = S {unS :: Int -> String}

toS :: S a -> S a
toS = id

instance Sym S where
  int x = S $ \_ -> "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  lam f = S $ \h ->
    let x = "x" ++ show h
    in  "lam (\\" ++ x ++ " -> " ++ unS (f (S (\_ -> x))) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e 0

instance Show (S a) where
  show = view

------------------------------------------------------------------------------
-- Expression tree

newtype E a = E {unE :: Int -> Tree}

instance Sym E where
  int x = E $ \_ -> Node "int" [Leaf $ show x]
  add e1 e2 = E $ \h -> Node "add" [unE e1 h, unE e2 h]
  lam f = E $ \h ->
    let x = "x" ++ show h
        ty = Node "TyInt" [] -- XXX: Fixed stub
        body = unE (f (E (\_ -> Leaf x))) h
    in  Node "lam" [Leaf x, ty, body]
  app e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]

tree :: E a -> Tree
tree e = unE e 0

------------------------------------------------------------------------------
-- Deserialize

-- data Term ty = forall r. Sym r => Ty (Ty ty) (r ty)
data Term r where
  Term :: Sym r => Ty ty -> r ty -> Term r

newtype W a = W {unW :: forall (r :: * -> *). Sym r => r a}

instance Sym W where
  int x = W (int x)
  add e1 e2 = W $ add (unW e1) (unW e2)
  -- lam f = W $ \x -> unW $ (f $ W x)
  -- lam :: (W a -> W b) -> W (a -> b)
  -- lam f = W $ \x -> unW $ f (W x) -- undefined
  app e1 e2 = W $ app (unW e1) (unW e2)

-- fromTree :: (Sym r) => Tree -> Either String (Term W)
fromTree :: Tree -> Either String (Term W)
fromTree e = case e of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Term TyInt (int x')
    Left err -> error $ err
  Node "add" [e1,e2] -> do
    Term TyInt e1' <- fromTree e1
    Term TyInt e2' <- fromTree e2
    return $ Term TyInt (add e1' e2')
  Node "var" [Leaf x] -> do
    undefined
  Node "lam" [v,ty,body] -> do
    undefined
  Node "app" [e1,e2] -> do
    undefined

e_to_s :: Tree -> Either String String
e_to_s e = do
  Term ty e' <- fromTree e
  return $ view $ unW e'
