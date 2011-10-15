{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://d.hatena.ne.jp/keigoi/20081226/p1>

Try:

> ghci> eval t02
> 1

-}
module HOAS1 where

data Term
  = Lam (Term -> Term)
  | App Term Term
  | Con Int

eval :: Term -> Term
eval e = case e of
  Lam _ -> e
  App e1 e2 -> case eval e1 of
    Lam f -> eval (f e2)
    Con _ -> error "App to Con"
  Con _ -> e

instance Show Term where
  show e = case e of
    Lam _ -> "<fun>"
    App e1 e2 -> show e1 ++ " " ++ show e2
    Con h -> show h

t01 = Lam (\x -> x)
t02 = App (Lam (\f -> App f (Con 1))) t01
