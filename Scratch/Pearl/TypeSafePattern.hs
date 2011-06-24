------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From 'FUNCTIONAL PEARL: Type-safe pattern combinators'. The goal of this
-- functional pearl is, to write functions which has haskell's builtin pattern
-- match feature.
--
-- /Example 1/: Shows compile time error
--
-- > match ((1,2),(3,4)) $
-- >  pair (cst 5) var ->> \x -> 2 + x
--
-- /Example 2/: Yields 7
--
-- > match (5,(3,4)) $
-- >   pair (cst 5) (pair var var) ->> \x y -> x + y
--
module Pearl.TypeSafePattern where

zero f = \() -> f
suc n f = \(x,xs) -> n (f x) xs

nil = \ks kf ac -> ks ac
cons v = \ks kf ac -> ks (v, ac)
m # n = \ks kf ac -> n (m ks kf) kf ac

-- fail
abort = \ks kf ac -> kf ()

-- catch
m `recover` n = \ks kf ac -> m ks (\() -> n ks kf ac) ac

var = (suc, \v -> cons v)
cst v' = (id, \v -> if v == v' then nil else abort)
pair p q = (curryPQ, \v -> matchP (fst v) # matchQ (snd v)) where
  (curryP, matchP) = p
  (curryQ, matchQ) = q
  curryPQ = curryP . curryQ

p ->> k = \v kf -> matchP v (curryP zero k) kf () where
  (curryP, matchP) = p

infixl 3 ->>

c1 ||: c2 = \v kf -> c1 v (\() -> c2 v kf)

infixr 2 ||:

match v cs = cs v (\() -> error "match")

p |-> k = \v -> match v $ p ->> k

none = (id, \v -> abort)

p \/ q = (curryPQ, \v -> matchP v `recover` matchQ v) where
  (curryP, matchP) = p
  (curryQ, matchQ) = q
  curryPQ = if True then curryP else curryQ

-- any, corresponds to builtin '_' in Haskell.
__ = (id, \v -> nil)

p /\ q = (curryP . curryQ, \v -> matchP v # matchQ v) where
  (curryP, matchP) = p
  (curryQ, matchQ) = q

p ? v = match v $ p ->> True ||: __ ->> False

is p = (id, \v -> if p v then nil else abort)

has p = (curryP, foldr recover abort . map matchP) where
  (curryP, matchP) = p

makeCompose z s = iterate where
  iterate n f x = match n $
    z     ->> x ||:
    s var ->> \n' -> iterate n' f (f x)

zInt = (id, \v -> if v == 0 then nil else abort)

sInt p = (curryP, \v -> if v == 0 then abort else matchP (v - 1)) where
  (curryP, matchP) = p

zList = (id, \v -> if null v then nil else abort)

sList p = (curryP, \v -> if null v then abort else matchP (tail v)) where
  (curryP, matchP) = p

compose_int = makeCompose zInt sInt

compose_list = makeCompose zList sList


--
-- Examples
--

ex05 :: Num a => a -> String
ex05 n = match n $
  cst 0 ->> "zero" ||:
  cst 1 ->> "one" ||:
  var   ->> \i -> error ("not a binary digit: " ++ show i)

ex07 :: Num a => [(a,a)] -> [a]
ex07 = map (pair var var |-> \x y -> x + y)

ex08 :: Num a => a
ex08 = match (99,1) $
  pair (cst 1) var \/ pair var (cst 1) ->> \x -> x

ex09 :: (Num a) => [(a,b)] -> [(a,b)]
ex09 = filter (pair (cst 0) __ ?)

ex10 :: (Num a, Integral b) => a -> b -> a
ex10 x n = match n $
  cst 0   ->> 1 ||:
  is even ->> square (ex10 x (n `div` 2)) ||:
  is odd  ->> x * ex10 x (n-1) where
    square x = x * x

ex11 :: (Integral n) => [(n,a)] -> [(n,a)]
ex11 = filter (pair (is even) __ ?)

ex12 :: Eq a => a -> [(a, t)] -> t
ex12 v = has (pair (cst v) var) |-> \x -> x

ex13 :: (t -> Bool) -> [t] -> t
ex13 f = has (var /\ is f) |-> \x -> x

get x = has (pair (cst x) var)

ex14 :: [(String, t)] -> t
ex14 env = match env $
  get "a" /\ get "A" ->> (\_ _ -> error "ambiguity") ||:
  get "a" \/ get "A" ->> (\a -> a) ||:
  __                 ->> error "unbound"