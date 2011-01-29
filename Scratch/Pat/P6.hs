{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Pattern DSL, take 2.
--
-- What I've found so far:
--
-- * Unless plam and papp get practical interpretation for R, it's doesn't seem
--   so much useful
--
-- * To show the pattern expression as String:
--
-- ** Make constructor function for each primitive type with specifying them,
--    e.g. pint :: Int -> p Int, pbool :: Bool -> p Bool, etc.
--
-- ** Make type signature for S as @S a = S {unS :: Int -> String}@, to get de
--    brujin notation for beta reduction of lam and app.
--
-- * Unless plam and papp are used, not so much need to separate primitive
--   constructor functions to its own class. plam and papp could not be
--   interpreted as String if @forall a. Show a => a -> String@ way of
--   definition is used.
--
module P6 where

import Control.Applicative
import Data.List
import System.Random

class Pprim p where
  pdouble :: Double -> p Double
  pint    :: Int -> p Int
  pbool   :: Bool -> p Bool
  pchar   :: Char -> p Char

class Pbool p where
  pif  :: p Bool -> p a -> p a -> p a
  peq  :: (Eq a) => p a -> p a -> p Bool

class Penum p where
  psucc :: (Enum a) => p a -> p a
  ppred :: (Enum a) => p a -> p a

class Ppair p where
  ppair :: p a -> p b -> p (a, b)

class Pval p where
  pval :: a -> p a

class Plambda p where
  plam :: (p a -> p b) -> p (a->b)
  papp :: p (a->b) -> p a -> p b

class Pfix p where
  pfix :: (p a -> p a) -> p a

class Pseq p where
  pseq :: p Int -> [p a] -> p a

class Pchoose p where
  pchoose :: p Int -> [p a] -> p a

------------------------------------------------------------------------------
-- R interpreter
------------------------------------------------------------------------------

newtype R a = R {unR :: StdGen -> [a]}

runP r g = unR r g
runPIO r = runP r `fmap` newStdGen

instance Pprim R where
  pdouble d = R $ \_ -> [d]
  pint i    = R $ \_ -> [i]
  pbool b   = R $ \_ -> [b]
  pchar c   = R $ \_ -> [c]

instance Pbool R where
  peq a b = R $ \g -> zipWith (==) (unR a g) (unR b g)
  pif p a b = R $ \g -> zipWith3 f (unR p g) (unR a g) (unR b g)
    where f q x y = if q then x else y

instance Penum R where
  psucc p = R $ \g -> map succ (unR p g)
  ppred p = R $ \g -> map pred (unR p g)

instance Ppair R where
  ppair a b  = R $ \g -> zip (unR a g) (unR b g)

instance Plambda R where
  plam f = R $ \g -> zipWith id (repeat func) (gens g)
    where
      func g' x = head $ unR (f (R $ \_ -> [x])) g'
  papp f p = R $ \g -> zipWith id (unR f g) (unR p g)

f4 :: (R a -> R b) -> R (a->b)
f4 f = R $ \g ->
  let foo :: [a->b]
      foo = undefined
      foo' :: [a->[b]]
      foo' = undefined
  in  foo

f5 :: [a->[b]] -> [a->b]
f5 fs = undefined

instance Pfix R where
  pfix f = f2 f

instance Pseq R where
  pseq n ps = R $ \g ->
    let ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
    in  concat $ zipWith unR ps' (gens g)

instance Pchoose R where
  pchoose n ps = R $ \g ->
    let gs = take (sum $ unR n g) (gens g)
    in  concatMap (\h -> let (j,_) = randomR (0,length ps - 1) h
                         in  unR (ps!!j) h) gs

gens g = iterate (snd . next) g

instance Functor R where
  fmap f r = R $ \g -> fmap f (unR r g)

instance Applicative R where
  pure x = R $ \_ -> [x]
  f <*> x = R $ \g -> zipWith id (unR f g) (unR x g)

instance (Show a) => Show (R a) where
  show _ = "R"

instance (Eq a) => Eq (R a) where
  (==) = undefined

instance (Num a) => Num (R a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = liftA abs
  negate = liftA negate
  signum = liftA signum
  fromInteger a = pure (fromInteger a)

instance (Fractional a) => Fractional (R a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational a = pure (fromRational a)

------------------------------------------------------------------------------
-- S interpreter
------------------------------------------------------------------------------

newtype S a = S {unS :: Int -> String}

showP p = unS p 0
printP = putStrLn . showP

instance Pprim S where
  pdouble d = S $ \_ -> "pdouble " ++ show d
  pint i    = S $ \_ -> "pint " ++ show i
  pbool b   = S $ \_ -> "pbool " ++ show b
  pchar c   = S $ \_ -> "pchar " ++ show c

instance Pbool S where
  peq a b   = S $ \h -> "peq (" ++ unS a h ++ ") (" ++ unS b h ++ ")"
  pif p a b = S $ \h ->
    "pif (" ++ unS p h ++ ") (" ++ unS a h ++ ") (" ++ unS b h ++ ")"

instance Penum S where
  psucc p = S $ \h -> "psucc (" ++ unS p h ++ ")"
  ppred p = S $ \h -> "ppred (" ++ unS p h ++ ")"

instance Ppair S where
  ppair a b  = S $ \h -> "ppair (" ++ unS a h ++ ") (" ++ unS b h ++ ")"

instance Plambda S where
  plam e     = S $ \h ->
    let x = "x" ++ show h
    in  "(\\" ++ x ++ " -> " ++ unS (e (S $ const x)) (succ h) ++ ")"
  papp e1 e2 = S $ \h -> "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

instance Pfix S where
  pfix e = S $ \h ->
    let self = "self" ++ show h
    in  "(pfix " ++ self ++ "." ++
        unS (e (S $ const self)) (succ h) ++ ")"

instance Pseq S where
  pseq n ps = S $ \h ->
    "pseq (" ++ unS n h ++ ") " ++
    "[" ++ (concat $ intersperse "," (map (\p -> unS p h) ps)) ++ "]"

instance Pchoose S where
  pchoose n ps = S $ \h ->
    "pchoose (" ++ unS n h ++ ") " ++
    "[" ++ (concat $ intersperse "," (map (\p -> unS p h) ps)) ++ "]"

instance (Show a) => Show (S a) where
  show p = unS p 0

instance (Eq a) => Eq (S a) where
  a == b = unS a 0 == unS b 0

instance (Num a) => Num (S a) where
  a + b = S $ \_ -> show a ++ " + " ++ show b
  a * b = S $ \_ -> show a ++ " * " ++ show b
  a - b = S $ \_ -> show a ++ " - " ++ show b
  abs a = S $ \_ -> "abs (" ++ show a
  negate a = S $ \_ -> "negate (" ++ show a
  signum a = S $ \_ -> "signum (" ++ show a
  fromInteger a = S $ \_ -> show a

instance (Fractional a) => Fractional (S a) where
  a / b = S $ \_ -> show a ++ " / " ++ show b
  recip a = S $ \_ -> "recip (" ++ show a ++ ")"
  fromRational a = S $ \_ -> show (fromRational a)

------------------------------------------------------------------------------
-- Sample patterns
------------------------------------------------------------------------------

p0 = ppair (pint 1) (pdouble 3)
p1 = plam (\x -> ppair (pdouble 1) x)

p2 = pseq (pint 5) [pint 1, pint 2, pint 3]
p3 = pchoose (pint 5) [pint 1, pint 2, pint 3]

p4 = plam (\x -> pseq x [pbool True, pbool False])
p5 = papp p4 (pchoose (pint 1) [pint 1, pint 2, pint 3])
p6 = plam (\x -> (plam (\y -> pseq x [y])))

-- scratch
f1 :: R a -> R a
f1 r = R $ \g -> concat $ foldr (\x xs -> unR x g:xs) [] (repeat r)

f2 :: (R a -> R a) -> R a
f2 f = R $ \g ->
  concat $ foldr (\x xs -> unR x g:xs) [] (iterate f (R $ const []))

f3 f = R $ \g -> undefined

newtype T a = T {unT :: a}

-- f3 f = R $ \g -> fx (unR . f . R) where fx f = f (fx f) -- undefined

fl :: (Eq a) => [a->a] -> a -> a
fl orig x = f orig x
  where
    f [] x = x
    f (a:as) x = if x == x' then f as x else f orig x' where x' = a x

p7 = pfix (\self ->
            plam (\n ->
                   pif (peq n (pint 0))
                     (pint 0)
                     (papp self (psucc n))))


a f = f g where g = f g