{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

* <http://d.hatena.ne.jp/keigoi/20081225/p1>

-}
module ExpP where

import Control.Monad.Reader

data ExpP v t where
  VarP :: v t -> ExpP v t
  ApP  :: ExpP v (a->b) -> ExpP v a -> ExpP v b
  LamP :: (v a -> ExpP v b) -> ExpP v (a->b)

newtype Exp t = Exp (forall v. ExpP v t)

toExpP :: ExpP v t -> ExpP v t
toExpP = id

eval :: Exp t -> t
eval (Exp e) = evalP e

newtype Prim a = Prim a

evalP :: ExpP Prim t -> t
evalP e = case e of
  VarP (Prim a) -> a
  ApP e1 e2 -> evalP e1 $ evalP e2
  LamP f -> evalP . f . Prim

newtype Var a = Var Int

showExp :: ExpP Var a -> ShowS
showExp t = runReader (showExpR t) 0 where
  showExpR :: ExpP Var a -> Reader Int ShowS
  showExpR e = case e of
    VarP (Var i) -> return $ \x -> "x" ++ shows i x
    ApP e1 e2    -> return $ \s -> showExp e1 $ " " ++ showExp e2 s
    LamP f       -> do
      i <- ask
      fstr <- local succ $ showExpR (f (Var i))
      return $ \s -> "\\x" ++ show i ++ " -> (" ++ fstr s ++ ")"

instance Show (ExpP Var t) where
  showsPrec _ e = showExp e

i01 = LamP $ \x -> (LamP $ \y -> ApP (VarP x) (VarP y))

------------------------------------------------------------------------------
-- Tagless final approach

class Sym r v where
  var :: v t -> r v t
  app :: r v (a->b) -> r v a -> r v b
  lam :: (v a -> r v b) -> r v (a->b)

f01 = lam (\x -> lam (\y -> app (var x) (var y)))
f02 = lam (\x -> var x)

-- Evaluates final expressions

newtype R (h :: * -> *) a = R {unR :: a}

newtype Id a = Id {unId :: a}

instance Sym R Id where
  var (Id x) = R $ x
  lam f = R $ \x -> unR (f (Id x))
  app e1 e2 = R $ (unR e1) (unR e2)

instance Show (R Id a) where
  show _ = "R"

instance Eq (R Id a) where
  _ == _ = error "Cannot compare R"

instance Num a => Num (R Id a) where
  R a + R b = R (a+b)
  R a * R b = R (a*b)
  R a - R b = R (a-b)
  abs (R a) = R $ abs a
  negate (R a) = R $ negate a
  signum (R a) = R $ signum a
  fromInteger x = R $ (fromInteger x)

n01 = lam (\x -> lam (\y -> var x + var y))
n02 = app (app n01 2) 3

-- | Evaluate R with Prim wrapper.
--
-- > ghci> evalR f01 succ 1
-- > 2
-- > ghci> evalR n02
-- > 5
--
evalR :: R Id a -> a
evalR = unR

-- | Show final expressions
newtype S (h :: * -> *) a = S {unS :: Int -> String}

-- | Showing string representation of syntax itself, defined in Sym class.
-- The only difference is variable names. Variables are named as 'xn', where
-- n is de Bruijn indice.
--
instance Sym S Var where
  var (Var i) = S $ \_ -> "var x" ++ show i
  lam f = S $ \h ->
    let x = S $ \h' -> "x" ++ (show $ succ h')
    in  "lam (\\x" ++ show h ++ " -> " ++ unS (f (Var h)) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

evalS :: S Var a -> String
evalS e = unS e 0

toS :: S Var a -> S Var a
toS = id

instance Eq (S Var a) where
  a == b = show a == show b

instance Num (S Var a) where
  e1 + e2 = S $ \h -> "(" ++ unS e1 h ++ " + " ++ unS e2 h ++ ")"
  e1 * e2 = S $ \h -> "(" ++ unS e1 h ++ " * " ++ unS e2 h ++ ")"
  e1 - e2 = S $ \h -> "(" ++ unS e1 h ++ " - " ++ unS e2 h ++ ")"
  negate e = S $ \h -> "(negate " ++ unS e h ++ ")"
  abs e = S $ \h -> "(abs " ++ unS e h ++ ")"
  signum e = S $ \h -> "(signum " ++ unS e h ++ ")"
  fromInteger x = S $ \h -> show (fromInteger x)

instance Show (S Var a) where
  show = evalS

------------------------------------------------------------------------------
-- Relating initial and final approach

instance Sym ExpP Var where
  var = VarP
  lam = LamP
  app = ApP

instance Sym ExpP Prim where
  var = VarP
  lam = LamP
  app = ApP

evar :: ExpP Var i -> ExpP Var i
evar = id

eprim :: ExpP Prim i -> ExpP Prim i
eprim = id
