{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 5.

-}
module Take05 where

import Take01 (Tree(..),ppTree,safeRead)

------------------------------------------------------------------------------
-- Class containing expression.

class Sym r h where
  v   :: h a -> r h a
  lam :: (h a -> r h b) -> r h (a->b)
  app :: r h (a->b) -> r h a -> r h b
  
  int :: Int -> r h Int
  add :: r h Int -> r h Int -> r h Int
  
e01 = add (int 1) (int 2)  
e02 = lam (\x -> v x)
e03 = lam (\x -> add (v x) (int 5))
e04 = app e03 (int 8)
e05 = lam (\x -> lam (\y -> add (v x) (v y)))
e06 = app (app e05 (int 24)) (int 75)
e07 = lam (\x -> app (v x) (int 8))
e08 = app e07 e03

------------------------------------------------------------------------------
-- Eval interpreter
  
newtype R (h :: * -> *) a = R {unR :: a}
newtype Id a = Id {unId :: a}

toR :: R Id a -> R Id a
toR = id

instance Sym R Id where
  v (Id x) = R x
  lam f = R $ \x -> unR (f (Id x))
  app e1 e2 = R $ (unR e1) (unR e2)
  
  int x = R x
  add e1 e2 = R $ (unR e1) + (unR e2)
  
eval :: R Id a -> a
eval e = unR e

------------------------------------------------------------------------------
-- View interpreter

newtype S (h :: * -> *) a = S {unS :: Int -> String}
newtype VarCount a = VarCount Int

toS :: S VarCount a -> S VarCount a
toS = id

instance Sym S VarCount where
  v (VarCount i) = S $ \_ -> "x" ++ show i
  lam f = S $ \h -> 
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS (f (VarCount h)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"
  
  int i = S $ \_ -> show i
  add e1 e2 = S $ \h -> unS e1 h ++ " + " ++ unS e2 h
  
view :: S VarCount a -> String  
view e = unS e 0

instance Show (S VarCount a) where
  show = view
  
------------------------------------------------------------------------------  
-- Syntax tree serializer
  
newtype E (h :: * -> *) a = E {unE :: Int -> Tree}

instance Sym E VarCount where
  v (VarCount i) = E $ \_ -> Node "var" [Leaf $ show i]
  lam f = E $ \h -> 
    let x = Leaf (show h) 
        -- XXX: Fixed as TyInt. Modify this.
        ty = Node "TyInt" []
        bdy = unE (f (VarCount h)) (succ h)
    in  Node "lam" [x,ty,bdy]
  app e1 e2 = E $ \h -> Node "app" [unE e1 h, unE e2 h]
  
  int x = E $ \_ -> Node "int" [Leaf $ show x]
  add e1 e2 = E $ \h -> Node "add" [unE e1 h, unE e2 h]

toE :: E VarCount a -> E VarCount a
toE = id

tree :: E VarCount a -> Tree
tree e = unE e 0

------------------------------------------------------------------------------
-- Deserialization

data Ty t where
  TyAny :: Ty a
  TyInt :: Ty Int
  TyArr :: Ty a -> Ty b -> Ty (a->b)
  
instance Show (Ty a) where  
  show ty = case ty of
    TyAny -> "any"
    TyInt -> "Int"
    TyArr ty1 ty2 -> "(" ++ show ty1 ++ " -> " ++ show ty2 ++ ")"
    
instance Eq (Ty a) where
  TyInt == TyInt = True
  TyAny == TyAny = True
  TyArr a1 a2 == TyArr b1 b2 = a1 == b1 && a2 == b2
  _ == _ = False
    
class T t where    
  tany :: t a
  tint :: t Int
  tarr :: t a -> t b -> t (a->b)
  
instance T Ty where tany = TyAny; tint = TyInt; tarr = TyArr
toTy :: Ty a -> Ty a
toTy = id
    
data ExtTy where ExtTy :: Ty ty -> ExtTy

data Equal a b where Equal :: Equal c c
                     
cmpTy :: Ty a -> Ty b -> Maybe (Equal a b)
cmpTy a b = case (a,b) of
  (TyInt,TyInt) -> Just Equal
  (TyArr a1 a2,TyArr b1 b2) -> do
    Equal <- cmpTy a1 b1
    Equal <- cmpTy a2 b2
    return Equal
  _ -> Nothing
    
data Term r h = forall ty. Sym r h => Term (Ty ty) (r h ty)

te01 = Term TyInt e01

te01_s = case Term TyInt e01 of Term TyInt t -> view t
te01_r = case Term TyInt e01 of Term TyInt t -> eval t
                                
-- | Strictly evaluate Ty and compare with cmpTy.
--
-- > ghci> te01_r_cmpTy 
-- > 3
--                                 
te01_r_cmpTy = 
  let ty = tint
  in  case Term ty e01 of 
    Term ty' t -> 
      case cmpTy ty ty' of
        Just Equal -> eval t

fromTree e g = case e of
  Node "int" [Leaf x] -> case safeRead x of
    Right x' -> return $ Term TyInt $ int x'
    Left err -> error err
  Node "add" [e1,e2] -> do
    Term TyInt val1 <- fromTree e1 g
    Term TyInt val2 <- fromTree e2 g
    return $ Term TyInt (add val1 val2)
  Node "var" [Leaf x] -> do
    undefined
    
e_to_s e = do
  Term ty e' <- fromTree (tree e) ()
  return $ (view e' ++ " :: " ++ show ty)
