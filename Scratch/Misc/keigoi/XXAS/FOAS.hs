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
module FOAS where

data Term1
  = Var String
  | Lam String Term1
  | App Term1 Term1
  | Con Int

f01 = Lam "x" (Var "x")
f02 = Lam "x" (Var "y")
