{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:
/Practical type inference for arbitrary-rank types/.
-}
module Scratch01 where

{-|
Below code would be rejected by haskell's type checker:

> foo :: ([Bool],[Char])
> foo =
>   let f x = (x [True,False], x ['a','b'])
>   in  f reverse

The type checker can assign to x the type `[Bool] -> [Bool]`,
or `[Char] -> [Char]`, but not `forall a. [a] -> [a]`.

We need Rank2Types or RankNTypes language pragma to let below code to work.

-}
foo :: ([Bool],[Char])
foo =
  let f :: (forall a. [a] -> [a]) -> ([Bool],[Char])
      f x = (x [True,False], x ['a','b'])
  in  f reverse

data Mon m = Mon { ret :: forall a. a -> m a
                 , bnd :: forall a b. m a -> (a -> m b) -> m b }

mapMon :: Mon m -> (a -> m b) -> [a] -> m [b]
mapMon m@(Mon {ret = ret', bnd = bnd'}) f xs = case xs of
  []     -> ret' []
  (y:ys) -> f y `bnd'` \z -> mapMon m f ys `bnd'` \zs -> ret' (z:zs)

------------------------------------------------------------------------------
-- Invariants

data Term v
  = Var v
  | App (Term v) (Term v)
  | Lam (Term (Incr v))
    deriving (Eq, Show)

data Incr v = Zero | Succ v deriving (Eq, Show)

foldT ::
  (forall a. a -> n a)
  -> (forall a. n a -> n a -> n a)
  -> (forall a. n (Incr a) -> n a)
  -> Term b -> n b
foldT = undefined

-- | Example of Rank-3 Types.
type MapT = forall a b. (a->b) -> Term a -> Term b

fixMT :: (MapT -> MapT) -> MapT
fixMT f = f (fixMT f)

mapT :: MapT
mapT = fixMT (\mt -> \f t ->
               case t of
                 Var x -> Var (f x)
                 App t1 t2 -> App (mt f t1) (mt f t2)
                 Lam t     -> Lam (mt (mapI f) t))

mapI = undefined

k :: forall a b. a -> b -> b
k = undefined

f1 :: (Int -> Int -> Int) -> Int
f1 = undefined

f2 :: (forall x. x -> x -> x) -> Int
f2 = undefined

-- Is (f1 k) well typed?
--
-- ghci> :t f1 k
-- f1 k :: Int
--
-- Yes, it is.

-- How about (f2 k)?
--
-- ghci> :t f1 k
-- f1 k :: Int
--
-- Yes, it is also.

g :: ((forall b. [b] -> [b]) -> Int) -> Int
g = undefined

k1 :: (forall a. a -> a) -> Int
k1 = undefined

k2 :: ([Int] -> [Int]) -> Int
k2 = undefined

-- Ill typed:
--
-- ghci> :t g k1
--
-- <interactive>:1:3:
--     Couldn't match expected type `forall b. [b] -> [b]'
--                 with actual type `forall a. a -> a'
--     Expected type: (forall b. [b] -> [b]) -> Int
--       Actual type: (forall a. a -> a) -> Int
--     In the first argument of `g', namely `k1'
--     In the expression: g k1
--
-- Ill typed? In paper, its written as `well typed`.
--
-- ghci> :t g k2
--
-- <interactive>:1:3:
--     Couldn't match expected type `forall b. [b] -> [b]'
--                 with actual type `[Int] -> [Int]'
--     Expected type: (forall b. [b] -> [b]) -> Int
--       Actual type: ([Int] -> [Int]) -> Int
--     In the first argument of `g', namely `k2'
--     In the expression: g k2

g2 :: ((forall a. a -> a) -> Int) -> Int
g2 = undefined

-- Well typed:
--
-- ghci> :t g2 k1
-- g2 k1 :: Int
--
-- Ill typed:
--
-- <interactive>:1:4:
--     Couldn't match expected type `forall a. a -> a'
--                 with actual type `[Int] -> [Int]'
--     Expected type: (forall a. a -> a) -> Int
--       Actual type: ([Int] -> [Int]) -> Int
--     In the first argument of `g2', namely `k2'
--     In the expression: g2 k2

revapp :: a -> (a->b) -> b
revapp x f = f x

poly :: (forall v. v -> v) -> (Int,Bool)
poly f = (f 3, f True)

{-
Road map
--------

Type contexts                Γ ::= Γ, x:σ | ∈
Polytypes                    σ ::= ∀ a.ρ
Rho-types (Rank 1)           ρ ::= τ
Rho-types (Arbitrary rank)   ρ ::= τ | σ → σ
Monotypes                    τ ::= Int | τ1 → τ2 | a
Type variables               a, b

-}

-- | Well typed.
h1 :: (forall a. a -> a) -> Int
h1 = (\(f::Int->Int) -> f 3)
