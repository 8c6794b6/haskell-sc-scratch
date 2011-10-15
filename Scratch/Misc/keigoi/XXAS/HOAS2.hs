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
module HOAS2 where

import Control.Monad.Reader

newtype VarE = VarE Char

data Term
  = Lam (VarE -> Term)
  | App Term Term
  | Con Int
  | Var VarE

instance Show Term where
  show t = runReader (showR t) 'a'

showR e = case e of
  Lam f -> do
    fresh <- ask
    body <- local succ (showR (f (VarE fresh)))
    return $ "(\\" ++ [fresh] ++ " -> " ++ body ++ ")"
  App e1 e2 -> do
    f1 <- showR e1
    f2 <- showR e2
    return $ f1 ++ " " ++ f2
  Con h -> return $ show h
  Var (VarE c) -> return [c]

eval :: Term -> Term
eval t = runReader (eval' t) ([],'a')

eval' :: Term -> Reader ([(Char,Term)],Char) Term
eval' e = case e of
  App e1 e2 -> do
    e1' <- eval' e1
    e2' <- eval' e2
    (_,x) <- ask
    case e1' of
      Lam f ->
        local (\(dic,var) -> ((x,e2'):dic,succ var)) (eval' (f (VarE x)))
      _     -> error "App to non function term"
  Var (VarE c) -> do
    (dic,_) <- ask
    case lookup c dic of
      Just a  -> return a
      Nothing -> error $ "Unbound variable: " ++ show c
  Con _ -> return e
  Lam _ -> return e

t01 = Lam (\x -> Var x)
t02 = App (Lam (\f -> App (Var f) (Con 1))) t01
t03 = Lam (\x -> Lam (\y -> App (Var x) (Var y)))
