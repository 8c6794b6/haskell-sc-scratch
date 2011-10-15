{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 6.

-}
module Take06 where

import Type01

class Sym r where
  v   :: Var -> r a
  lam :: (Var -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b

  int :: Int -> r Int
  add :: r Int -> r Int -> r Int

-- Although Var type is used inside v and lam in Sym class,
-- Sym expressions does not need to refer to VZ nor VS explicitly.
--

e01 = add (int 3) (int 4)
e02 = lam (\x -> v x)
e03 = lam (\x -> lam (\y -> add (v x) (v y)))
e04 = e03 `app` int 27 `app` int 72
e05 = lam (\x -> app (v x) (int 3))
e06 = lam (\x -> add (v x) (int 4))
e07 = app e05 e06

-- | De Bruijn indice.
data Var = VZ | VS Var deriving (Eq,Show)

newtype Dbj a = Dbj Var

data Variable where
  Variable :: Show t => t -> Variable

instance Show Variable where
  show (Variable t) = show t

data Vars where
  Nil  :: Vars
  Cons :: Variable -> Vars -> Vars

lkp :: Var -> Vars -> Variable
lkp i vs = case (i,vs) of
  (VZ,    Cons v _)  -> v
  (VS i', Cons _ vs) -> lkp i' vs
  _                  -> error "Variable unbound"

-- instance Variables () where
--   type Value () = ()
--   findv _ _ = Left "Unbound variable"

-- instance Variables g => Variables (t,g) where
--   type Value (t,g) = t
--   findv dbj (a,g) = case dbj of
--     VZ      -> return $ v VZ
--     VS dbj' -> VS $ findv dbj' g

{-
In Sym class, ther exists fixed use of Var type class in v and lam methods.
How to get result in arbitrary types with this fixed Var type?
-}
newtype R a = R {unR :: Vars -> a}

data RTerm = forall a. RTerm (R a)

rv x = R $ \h -> lookupR x h

class VarEnv g where
  type Value g :: *
  lookupVE :: Var -> g -> Value g

instance VarEnv () where
  type Value () = ()
  lookupVE _ _ = error "Unbound variable"

instance VarEnv g => VarEnv (t,g) where
  type Value (t,g) = (t,Value g)
  lookupVE v (t,g) = case v of
    VZ    -> (t,undefined)
    VS v' -> undefined -- lookupVE v' g

lookupR v h = undefined
--   (VZ,t)    -> return t
--   (VS v,ts) -> lookupR v ts

instance Sym R where
  int x = R $ \_ -> x
  add e1 e2 = R $ \h -> unR e1 h + unR e2 h
  v x       = R $ \h -> undefined
  -- v x = case lkup x h of
  --   Variable a  -> a
  --   _           -> error $ "Unbound variable: " ++ show x
  lam f     = R $ \h -> undefined
  app e1 e2 = R $ \h -> undefined

lkup :: Var -> Vars -> Variable
lkup = lkp

------------------------------------------------------------------------------
-- Show interpreter, working.

newtype S a = S {unS :: Var -> String}

toS :: S a -> S a
toS = id

instance Sym S where
  int x = S $ \_ -> show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

  v x = S $ \h -> go x 0
  lam f = S $ \h ->
    let x = go h 0
    in  "lam (\\" ++ x ++ " -> " ++ unS (f h) (VS h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"

go :: Var -> Int -> String
go y i = case y of VZ -> "x" ++ show i; VS z -> go z (succ i)

view :: S a -> String
view e = unS e VZ

instance Show (S a) where
  show = view