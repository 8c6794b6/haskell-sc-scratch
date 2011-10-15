{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://d.hatena.ne.jp/keigoi/20081226/p1>
-}
module PHOAS where

import Control.Monad.Reader

-- | Untyped lambda calsulus, or typed lambda calsulus with single type.
data Term v
  = Var v
  | Lam (v -> Term v)
  | App (Term v) (Term v)
  | Con Int

newtype VarE = VarE Char

instance Enum VarE where
  succ (VarE x) = VarE (succ x)
  pred (VarE x) = VarE (pred x)
  toEnum i = VarE (toEnum i)
  fromEnum (VarE x) = fromEnum x

instance Show (Term VarE) where
  show t = runReader (showR t) (VarE 'a')

toTV :: Term VarE -> Term VarE
toTV = id

showR :: Term VarE -> Reader VarE String
showR t = case t of
  Lam f -> do
    x <- ask
    let body = f x
        VarE c = x
    bodystr <- local succ $ showR body
    return $ "(\\" ++ [c] ++ " -> " ++ bodystr ++ ")"
  App t1 t2 -> do
    t1' <- showR t1
    t2' <- showR t2
    return $ t1' ++ " " ++ t2'
  Con i -> return $ show i
  Var (VarE x) -> return $ [x]

newtype Id = Id (Term Id)

eval :: Term Id -> Int
eval t = case eval' t of Con i -> i

-- | We have a chance to evaluate Con in app body.
eval' :: Term Id -> Term Id
eval' e = case e of
  Lam _ -> e
  App t1 t2 -> case eval' t1 of
    Lam f -> eval' (f (Id $ eval' t2))
    _     -> error "eval: App was not a function"
  Con _ -> e
  Var (Id t) -> t

t01 = Lam (\x -> Var x)
t02 = Lam (\f -> App (Var f) (Con 1))
t03 = App t02 t01
t04 = Lam (\x -> Lam (\y -> App (Var x) (Var y)))
t05 = App (App t04 t01) (Con 1)

-- GHC enables to write below.
f01 = Lam (\x -> App (Con 1) (Var x))