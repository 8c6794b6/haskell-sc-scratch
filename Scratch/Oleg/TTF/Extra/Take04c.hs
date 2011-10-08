{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 4, pattern c.

TODO: Remove z and s from target language.

-}
module Take04c where

import Control.Monad.Instances ()

import Type01

class Sym r where
  lam :: r (a,h) b -> r h (a->b)
  app :: r h (a->b) -> r h a -> r h b
  int :: Int -> r h Int
  add :: r h Int -> r h Int -> r h Int

class Var r where
  z :: r (a,h) a
  s :: r h a -> r (any,h) a

t01 = add (int 1) (int 2)
t02 = lam z
t03 = lam (add z z)
t04 = lam (lam (add z (s z)))
t05 = t04 `app` int 72 `app` int 27

------------------------------------------------------------------------------
-- Eval interpreter

newtype R h a = R {unR :: h -> a}

instance Sym R where
  lam f = R $ \h -> \x -> unR f (x,h)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)
  int x = R $ const x
  add e1 e2 = R $ \h -> unR e1 h + unR e2 h

instance Var R where
  z   = R $ \(x,_) -> x
  s x = R $ \(_,h) -> unR x h

eval e = unR e ()

------------------------------------------------------------------------------
-- Show interpreter

newtype S h a = S {unS :: Int -> String}

toS :: S h a -> S h a
toS = id

instance Sym S where
  lam f = S $ \h ->
    let x = show h
    in  "lam (\\x" ++ show h ++ " -> " ++ unS f (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  int x = S $ const $ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

instance Var S where
  z = S $ \h -> "x" ++ show (pred h)
  s v = S $ \h -> unS v (pred h)

view :: S h a -> String
view e = unS e 0

instance Show (S h a) where
  show = view

------------------------------------------------------------------------------
-- Syntax tree serialize interpreter

newtype E h a = E {unE :: Int -> Tree}

tree :: E h a -> Tree
tree e = unE e 0

instance Sym E where
  lam f = E $ \h ->
    let x = show h
        -- XXX: Constantly TyInt. Resolve the type from body node.
        ty = Node "TyInt" []
        body = unE f (succ h)
    in  Node "lam" [Leaf x,ty,body]
  app e1 e2 = E $ \h ->
    Node "app" [unE e1 h, unE e2 h]
  int x = E $ \_ -> Node "int" [Leaf $ show x]
  add e1 e2 = E $ \h -> Node "add" [unE e1 h, unE e2 h]

instance Var E where
  z   = E $ \h -> Node "var" [Leaf $ show (pred h)]
  s v = E $ \h -> unE v (pred h)

------------------------------------------------------------------------------
-- Deserialize

data Term r h where
  Term :: Ty a -> r h a -> Term r h

data VarDesc t = VarDesc (Ty t) String

class VarEnv gamma h | gamma -> h where
  findvar :: (Sym r, Var r) =>
             String -> gamma -> Either String (Term r h)

instance VarEnv () () where
  findvar n _ = error $ "Unbound variable: " ++ n

instance VarEnv gma h => VarEnv (VarDesc t,gma) (t,h) where
  findvar n (VarDesc ty n',_) | n == n' = return $ Term ty z
  findvar n (_,gma) = do
    Term ty v <- findvar n gma
    return $ Term ty (s v)

tree_to_s_io :: Tree -> IO ()
tree_to_s_io e = case fromTree e () of
  Right (Term ty t) -> do
    let t' = unW t
    putStrLn $ view t' ++ " :: " ++ show ty

tree_to_r_int :: Monad m => Tree -> m Int
tree_to_r_int = tree_to_r_ty TyInt

tree_to_r_ty :: Monad m => Ty t -> Tree -> m t
tree_to_r_ty ty e = case fromTree e () of
  Right (Term ty' t) -> case cmpTy ty' ty of
    Just Equal -> return $ eval (unW t)
    Nothing    -> error $ "Type mismatch: " ++ show ty'

-- | XXX: /Cannot use type class?/
--
-- When type signature of fromTree was defined as:
--
-- > fromTree ::
-- >   (Sym repr, Var repr, VarEnv gamma h)
-- >   => Tree -> gamma -> Either String (Term repr h)
--
-- GHC complain with below:
--
-- > Couldn't match type `repr1' with `repr'
-- >   `repr1' is untouchable
-- >           inside the constraints (a1 ~ Int)
-- >           bound at a pattern with constructor
-- >                      TyInt :: Ty Int,
-- >                    in a pattern binding in
-- >                         a 'do' expression
-- >   `repr' is a rigid type variable bound by
-- >          the type signature for
-- >            fromTree :: (Sym repr, Var repr, VarEnv gamma h) =>
-- >                        Tree -> gamma -> Either String (Term repr h)
--
-- ... Using W for fixing type.
--
fromTree :: VarEnv gamma h => Tree -> gamma -> Either String (Term W h)
fromTree e gma = case e of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Term TyInt (int x')
    Left err -> error err
  Node "add" [e1,e2] -> do
    Term TyInt t1 <- fromTree e1 gma
    Term TyInt t2 <- fromTree e2 gma
    return $ Term TyInt (add t1 t2)
  Node "lam" [Leaf v,ty,body] -> do
    extty@(ExtTy argty) <- treeToTy ty
    Term bodyty bodyval <- fromTree body ((VarDesc argty v),gma)
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
-- Closed interpreter for all Sym instances.

newtype W h a = W {unW :: forall r. (Var r, Sym r) => r h a}

instance Sym W where
  int x     = W $ int x
  add e1 e2 = W $ add (unW e1) (unW e2)
  lam e     = W $ lam (unW e)
  app e1 e2 = W $ app (unW e1) (unW e2)

instance Var W where
  z         = W z
  s v       = W $ s (unW v)
