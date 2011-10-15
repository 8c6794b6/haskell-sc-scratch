{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 4, pattern c.
Using type families instead of functional dependencies.

TODO: Remove z and s from target language.

-}
module Take04d where

import Type01
import Take04c
  (Sym(..),Var(..),R(..),eval,S(..),view,E(..),tree,W(..),Term(..))

------------------------------------------------------------------------------
-- Deserialize, in TypeFamily and simplified type representation.

data VarDesc t = VarDesc String (Ty t)

class VarEnv g where
  type Value g
  findvar :: (Sym r, Var r) => String -> g -> Either String (Term r (Value g))

instance VarEnv () where
  type Value () = ()
  findvar n _ = error $ "Unbound variable: " ++ n

instance VarEnv g => VarEnv (VarDesc t,g) where
  type Value (VarDesc t,g) = (t,Value g)
  findvar n (VarDesc n' ty,g)
    | n == n'   = return $ Term ty z
    | otherwise = findvar n g >>= \(Term ty v) -> return $ Term ty (s v)

-- | Guts of deserializer. Result type is fixed to W.
fromTree :: VarEnv g => Tree -> g -> Either String (Term W (Value g))
fromTree e gma = case e of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Term TyInt (int x')
    Left err -> error err
  Node "add" [e1,e2] -> do
    Term TyInt t1 <- fromTree e1 gma
    Term TyInt t2 <- fromTree e2 gma
    return $ Term TyInt (add t1 t2)
  Node "lam" [Leaf v,ty,body] -> do
    ExtTy argty <- treeToTy ty
    Term bodyty bodyval <- fromTree body ((VarDesc v argty),gma)
    return $ Term (TyArr argty bodyty) (lam bodyval)
  Node "var" [Leaf v] -> findvar v gma
  Node "app" [e1,e2] -> do
    Term (TyArr bndty bodyty) e1' <- fromTree e1 gma
    Term argty e2' <- fromTree e2 gma
    case cmpTy argty bndty of
      Just Equal -> return $ Term bodyty (app e1' e2')

treeToTy :: Monad m => Tree -> m ExtTy
treeToTy e = case e of
  Node "TyInt" [] -> return (ExtTy TyInt)
  Node "TyArr" [e1,e2] -> do
    ExtTy ty1 <- treeToTy e1
    ExtTy ty2 <- treeToTy e2
    return $ ExtTy $ TyArr ty1 ty2

------------------------------------------------------------------------------
-- Tests

tree_to_s_io :: Tree -> IO ()
tree_to_s_io e = case fromTree e () of
  Right (Term ty t) -> do
    let t' = unW t
    putStrLn $ view t' ++ " :: " ++ show ty

tree_to_r_ty :: Monad m => Ty t -> Tree -> m t
tree_to_r_ty ty e = case fromTree e () of
  Right (Term ty' t) -> case cmpTy ty' ty of
    Just Equal -> return $ eval (unW t)
    Nothing    -> error $ "Type mismatch: " ++ show ty'

tree_to_r_int :: Monad m => Tree -> m Int
tree_to_r_int = tree_to_r_ty TyInt

tree_to_r_int_io :: Tree -> IO ()
tree_to_r_int_io e = print =<< tree_to_r_int e

t01 = add (int 1) (int 2)
t02 = lam z
t03 = lam (add z z)
t04 = lam (lam (add z (s z)))
t05 = t04 `app` int 72 `app` int 27

tests = do
  let ss = [tree t01,tree t02,tree t03,tree t04,tree t05]
  mapM_ tree_to_s_io ss
  let rs = [tree t01,tree t05]
  mapM_ tree_to_r_int_io rs

main = tests