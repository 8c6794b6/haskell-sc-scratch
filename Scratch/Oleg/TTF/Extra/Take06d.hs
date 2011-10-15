{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 6, variation d.
Using Data.Typeable and cast to get value from variable environment.

-}
module Take06d where

import Data.Typeable
import Control.Monad.Instances

class Sym r where
  var :: Typeable a => Var a -> r a
  lam :: (Typeable a, Typeable b) => (Var a -> r b) -> r (a->b)
  app :: (Typeable a, Typeable b) => r (a->b) -> r a -> r b

  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  mul :: r Int -> r Int -> r Int

newtype Var a = Var VIdx deriving Show

unVar :: Var a -> VIdx
unVar (Var i) = i

data VIdx = VZ | VS VIdx deriving (Show, Read, Typeable)

vidx :: VIdx -> Int
vidx v = case v of VZ -> 0; VS v' -> succ (vidx v')

t01 = add (int 1) (int 2)
t02 = lam (\x -> var x)
t03 = lam (\x -> lam (\y -> var x))
t03a = app (app t03 (int 1)) (int 2)
t04 = lam (\x -> app (var x) (int 3))
t05 = app t04 t02
t06 = lam (\x -> lam (\y -> var y))
t06a = app (app t06 (int 1)) (int 2)
t07 = lam (\x -> lam (\y -> add (var x) (var y)))
t07a = app (app t07 (int 3)) (int 5)

t11 = mul (int 2) (int 3)
t12f = lam (\x -> lam (\y -> lam (\z -> add (var x) (var y) `mul` var z)))

------------------------------------------------------------------------------
-- Eval interpreter

newtype R a = R {unR :: Typeable a => (VIdx,VarEnv) -> a}

toR :: R a -> R a
toR = id

data VarEnv where
  Nil  :: VarEnv
  Cons :: Typeable a => a -> VarEnv -> VarEnv

lkup :: Typeable a => VIdx -> VarEnv -> Maybe a
lkup v g = case (v,g) of
  (VZ   ,Cons a  _) -> cast a
  (VS v',Cons _ g') -> lkup v' g'
  (_    ,Nil      ) -> Nothing

instance Sym R where
  var x     = R $ \(_,g) -> case lkup (unVar x) g of
    Just y  -> y
    Nothing -> error $ "Unbound var: " ++ show x
  lam f     = R $ \(i,g) -> \x -> unR (f (Var i)) (VS i,Cons x g)
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)
  int x     = R $ const x
  add e1 e2 = R $ \h -> unR e1 h + unR e2 h
  mul e1 e2 = R $ \h -> unR e1 h * unR e2 h

eval :: Typeable a => R a -> a
eval e = unR e (VZ,Nil)

------------------------------------------------------------------------------
-- View interpreter
--
-- When expression is not written in fixed type, need to specify type to view.
-- Cannot view a expression when 'Typeable a => a' remains in its signature.
--

newtype S a = S {unS :: VIdx -> String}

toS :: S a -> S a
toS = id

instance Show (S a) where show = view

instance Sym S where
  var x = S $ \_ -> "var x" ++ show (vidx $ unVar x)
  lam f = S $ \h ->
    let x = "x" ++ show (vidx h)
    in  concat ["lam (\\",x," -> ",unS (f (Var h)) (VS h),")"]
  app e1 e2 = S $ \h -> concat ["app (",unS e1 h,") (",unS e2 h,")"]
  int x = S $ \_ -> "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  mul e1 e2 = S $ \h -> "mul (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e VZ

------------------------------------------------------------------------------
-- Expression tree interpreter
--
-- Again, need to resolve types.

newtype E a = E {unE :: VIdx -> Tree}

data Tree = Leaf String | Node String [Tree] deriving (Eq,Show)

instance Sym E where
  var x = E $ \_ -> Node "var" [Leaf (show $ unVar x)]
  lam f = E $ \h ->
    let x = Leaf (show $ h)
        ty = Node "TyInt" []
        body = unE (f (Var h)) (VS h)
    in  Node "lam" [x,ty,body]
  app e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]
  int x = E $ \h -> Node "int" [Leaf (show x)]
  add e1 e2 = E $ \h -> Node "add" [unE e1 h, unE e2 h]
  mul e1 e2 = E $ \h -> Node "mul" [unE e1 h, unE e2 h]

tree :: E a -> Tree
tree e = unE e VZ

------------------------------------------------------------------------------
-- Can we deserialize?

data Ty t where
  TyInt :: Ty Int
  TyArr :: Ty a -> Ty b -> Ty (a->b)

instance Show (Ty t) where
  show TyInt = "TyInt"
  show (TyArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"

data Term r where
  Term :: Sym r => Ty t -> r t -> Term r

data Equal a b where
  Equal :: Equal c c

cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy t1 t2 = case (t1,t2) of
  (TyInt,TyInt) -> Just Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal

safeRead :: Read a => String -> Either String a
safeRead str = case reads str of
  [(a,[])] -> Right a
  _        -> Left $ "Failed reading: " ++ str

fromTree t e g = case e of
  Node "int" [Leaf e] -> case safeRead e of
    Right x -> return $ Term TyInt (int x)
  Node "add" [e1,e2] -> do
    Term t1 e1' <- fromTree t e1 g
    Term t2 e2' <- fromTree t e2 g
    case (cmpTy t1 TyInt,cmpTy t2 TyInt) of
      (Just Equal,Just Equal) -> return $ Term TyInt (add e1' e2')
      _ -> Left "add: type mismatch"
  Node "mul" [e1,e2] -> do
    Term t1 e1' <- fromTree t e1 g
    Term t2 e2' <- fromTree t e2 g
    case (cmpTy t1 TyInt,cmpTy t2 TyInt) of
      (Just Equal,Just Equal) -> return $ Term TyInt (mul e1' e2')
  Node "var" [Leaf v] -> do
    return $ Term t (var (Var (read v)))
    -- let v' = read v
    --     getTy :: Ty t -> t
    --     getTy = undefined
    -- case lkup v' g `asTypeOf` Just (getTy t) of
    --   Just y -> return $ Term t (var (Var v'))
  Node "lam" [Leaf v,ty,body] -> do
    undefined


------------------------------------------------------------------------------
-- Tests

-- e_to_s :: E a -> String
-- e_to_s t = case fromTree (tree t) Nil of Right (Term _ t') -> view t'

-- e_to_r_int :: E Int -> Int
-- e_to_r_int t = case fromTree (tree t) Nil of
--   Right (Term TyInt t' :: Term R) -> eval $ toR t'

-- main = do
--   print $ (e_to_s t01, show $ e_to_r_int t01)
--   print $ (e_to_s t11, show $ e_to_r_int t11)
