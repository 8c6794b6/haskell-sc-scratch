{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

To deserialize lambda expression, take 4, pattern b.

-}
module Take04b where

import Text.ParserCombinators.Parsec

import Type01

class Sym r where
  var :: a -> r a
  lam :: (r a -> r b) -> r (a->b)
  app :: r (a->b) -> r a -> r b
  
  int :: Int -> r Int
  add :: r Int -> r Int -> r Int
  
e01 = add (int 1) (int 2)
e02 = lam (\x -> x)
e03 = app e02 (int 1)
e04 = lam (\x -> add x x)
e05 = app e04 (int 8)
e06 = lam (\x -> lam (\y -> add x y))
e07 = app (app e06 (int 1)) (int 2)
e08 = lam (\x -> app x (int 7))
e09 = app e08 e04
e10 = app e02 (var False)

newtype R a = R {unR :: a}

instance Sym R where
  var = R
  lam f = R $ \x -> unR (f (R x))
  app e1 e2 = R $ (unR e1) (unR e2)
  int x = R x
  add e1 e2 = R $ unR e1 + unR e2
  
eval :: R a -> a
eval = unR

-- newtype U a = U {unU :: Uval}

-- data Uval
--   = UInt Int
--   | UArr (Uval -> Uval)
    
-- instance Show Uval where    
--   show (UInt i) = "UInt " ++ show i
--   show (UArr _) = "UArr <function>"

-- data UVar = UZ | US UVar

-- instance Sym U where
--   var = undefined
--   int = UInt
--   add e1 e2 = case unU e1 of
--     UInt a1 -> case unU e2 of
--       UInt a2 -> UInt (e1 + e2)
--   lam f = UArr (\x -> unU f (U x))
--   app e1 e2 = case unU e1 of
--     UArr f -> f (unU e2)
    
newtype S a = S {unS :: Int -> String}

toS :: S a -> S a
toS = id

instance Show (S a) where
  show = view

instance Sym S where
  var _ = S $ \h -> "x" ++ show h
  lam f = S $ \h -> 
    let x = "x" ++ show h
        vx = S $ const x
    in  "lam (\\" ++ x ++ " -> " ++ unS (f vx) (succ h) ++ ")"
  app e1 e2 = S $ \h -> "app (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  int x = S $ \_ -> "int " ++ show x
  add e1 e2 = S $ \h -> "add (" ++ unS e1 h ++ ") (" ++ unS e2 h ++ ")"
  
view :: S a -> String
view e = unS e 0

data Term r where 
  Term :: Sym r => Ty a -> r a -> Term r
  
sym = try pint <|> try padd <|> try plam -- <|> pvar <|> plam <|> papp
  
pvar = do 
  char 'x'
  idx <- many digit 
  return $ lkup ('x':idx)
  -- return $ lkup ('x':idx)
  
lkup key = undefined

-- plam = do
--   string "lam" 
--   Term body <- between (char '(') (char ')') $ do
--     string "\\x"
--     idx <- many digit 
--     skipMany1 space
--     string "->"
--     skipMany1 space
--     sym
--   return $ Term (lam body)
  
plam = do
  string "lam"
  skipMany1 space
  string "(\\x" 
  idx <- many1 digit
  skipMany1 space
  string "->"
  skipMany1 space
  Term (TyArr TyInt TyInt) bdy <- sym
  char ')'
  return undefined
  -- return $ Term $ lam bdy
  
papp = undefined

pint = do
  string "int"
  skipMany1 space
  n <- many digit
  return $ Term TyInt $ int $ read n

padd = do
  string "add"
  skipMany1 space
  Term TyInt e1 <- braced sym 
  skipMany1 space
  Term TyInt e2 <- braced sym
  return $ Term TyInt $ add e1 e2
  
braced p = between (char '(') (char ')') p  
  
-- | Works:
-- 
-- > ghci> s_to_r $ view e01
-- > 3
-- 
s_to_r s = case parse sym "" s of 
  Right (Term TyInt x) -> eval x
  _                    -> error "Fail"

