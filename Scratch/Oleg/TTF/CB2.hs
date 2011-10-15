{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

From: <http://okmij.org/ftp/tagless-final/CB.hs>, variation 2.
-}
module CB2 where

import Control.Monad
import Control.Monad.Trans

data IntT
data BoolT
data a :-> b
infixr 5 :->

class EDSL exp where
  lam :: (exp a -> exp b) -> exp (a :-> b)
  app :: exp (a :-> b) -> exp a -> exp b
  int :: Int -> exp IntT
  add :: exp IntT -> exp IntT -> exp IntT

let_ :: EDSL exp => exp a -> (exp a -> exp b) -> exp b
let_ x y = (lam y) `app` x

t01 :: EDSL exp => exp IntT
t01 = (lam $ \x -> let_ (x `add` x)
                   $ \y -> y `add` y) `app` int 10

class MulL exp where
  mul :: exp IntT -> exp IntT -> exp IntT

class BoolL exp where
  bool :: Bool -> exp BoolT
  leq  :: exp IntT -> exp IntT -> exp BoolT
  if_  :: exp BoolT -> exp a -> exp a -> exp a

type family Sem (m :: * -> *) a :: *
type instance Sem m IntT = Int
type instance Sem m BoolT = Bool
type instance Sem m (a :-> b) = m (Sem m a) -> m (Sem m b)

newtype S l m a = S {unS :: m (Sem m a)}

newtype Id a = Id a deriving (Show,Functor)

instance Monad Id where
  return = Id
  Id a >>= k = k a

unId :: Id a -> a
unId (Id a) = a

data Void

instance Monad m => EDSL (S Void m) where
  int = S . return
  add x y = S $ do
    a <- unS x
    b <- unS y
    return $ a + b
  lam f = S . return $ (unS . f . S)
  app x y = S $ unS x >>= ($ (unS y))

runVoid :: S Void Id a -> Id (Sem Id a)
runVoid x = unS x

newtype View m = View {unView :: Int -> String}

toView :: View m -> View m
toView = id

instance Show (View m) where
  show = view

instance EDSL View where
  int x = View $ \_ -> "int " ++ show x
  add x y = View $ \h -> "add (" ++ unView x h ++ ") (" ++ unView y h ++ ")"
  lam f = View $ \h ->
    let x = "x" ++ show h
    in  "lam (\\" ++ x ++ " -> " ++ unView (f (View $ const x)) (succ h) ++ ")"
  app x y = View $ \h -> "app (" ++ unView x h ++ ") (" ++ unView y h ++ ")"

view :: View m -> String
view e = unView e 0
