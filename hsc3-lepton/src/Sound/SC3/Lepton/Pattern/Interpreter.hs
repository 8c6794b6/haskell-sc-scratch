{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (Rank2Types, FlexibleContexts)
--
-- Interpreter of pattern DSL.
--
module Sound.SC3.Lepton.Pattern.Interpreter
  ( -- * Running patterns
    R(..), runP, runPIO,

    -- * Showing patterns
    S(..), showP,

    -- -- Viewing patterns
    -- V(..), viewP,
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Data (Typeable1(..), mkTyCon, mkTyConApp)
import Data.Map (Map)
import System.Random (StdGen, Random(..), RandomGen, next, newStdGen)
import qualified Data.Map as M

import Sound.SC3.Lepton.Pattern.Expression

------------------------------------------------------------------------------
--
-- R interpreter
--
------------------------------------------------------------------------------

-- | \"R\" for running patterns.
--
-- Note that enumeration for floating point values are not working,
--
-- e.g: When getting single random element from @[0.1,0.2 .. 2.0]@, below will
-- not work:
--
-- > runPIO $ prand 1 [0.1,0.2..2.0]
---
-- In above case, explicitly use pattern language to make desired value:
--
-- > runPIO $ prand 1 $ map pval [0.1,0.2..1.0]
--
newtype R a = R {unR :: StdGen -> [a]}

-- | Run pattern with given seed.
runP :: R a -> StdGen -> [a]
runP p g = unR p g

-- | Run pattern with new seed.
runPIO :: R a -> IO [a]
runPIO p = unR p `fmap` newStdGen

instance (Eq a) => Eq (R a) where
  -- undefined !!
  (==) = undefined

instance (Show a) => Show (R a) where
  show _ = "R"

instance Functor R where
  fmap f (R r) = R $ \g -> fmap f (r g)

instance Monad R where
  return a = R $ \_ -> [a]
  (R r) >>= k = R $ \g -> concatMap (\x -> unR (k x) g) (r g)

-- | Behaves same as ZipList.
instance Applicative R where
  pure x = R $ \_ -> repeat x
  R rf <*> R rv = R $ \g -> zipWith id (rf g) (rv g)

instance Typeable1 R where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.R") []

instance (Num a) => Num (R a) where
  a + b = (+) <$> a <*> b
  a * b = (*) <$> a <*> b
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

instance (Fractional a) => Fractional (R a) where
  a / b = (/) <$> a <*> b
  fromRational = return . fromRational

instance (Enum a) => Enum (R a) where
  succ = fmap succ
  pred = fmap pred
  fromEnum (R r)  = fromEnum $ head (r undefined)
  toEnum = return . toEnum

--
-- Instance for expressions
--

-- | Singleton list.
instance Pval R where
  pval a = R $ \_ -> [a]

-- | Returns @[]@.
instance Pempty R where
  pempty = R $ \_ -> []

instance Plist R where
  plist a = R $ \_ -> a

instance Pconcat R where
  pconcat ps = R $ \g -> concat $ zipWith unR ps (gens g)

-- | Repeats the list of pattern with given number.
instance Pseq R where
  pseq n ps = R $ \g ->
    let ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
    in  concat $ zipWith unR ps' (gens g)

-- | Replicate given pattern for given time.
instance Preplicate R where
  preplicate n p = R $ \g ->
    let ps = concatMap (\n' -> replicate n' p) (unR n g)
    in  concat $ zipWith unR ps (gens g)

-- | Choose element from given list for number of given times.
instance Prand R where
  prand n ps = R $ \g ->
    let gs = take (sum $ unR n g) (gens g)
    in  concatMap (\h -> let (j,_) = randomR (0,length ps - 1) h
                         in  unR (ps!!j) h) gs

-- lo and hi bounds won't vary with: randomRs (lo, hi) g
instance Prange R where
  prange lo hi =  R $ \g ->
    zipWith3 (\l h g' -> fst $ randomR (l,h) g') (unR lo g) (unR hi g) (gens g)

instance Prandom R where
  prandom = R randoms

instance Pchoose R where
  pchoose = prand

instance Pcycle R where
  pcycle ps = R $ \g -> concat $ zipWith unR (cycle ps) (gens g)

instance Prepeat R where
  prepeat a = R $ \_ -> repeat a

instance Ploop R where
  ploop p = R $ \g -> concatMap (unR p) (gens g)

-- | Same as @(<*>)@.
instance Papp R where
  papp = (<*>)

instance Plam R where
  plam = rlam

rlam :: (R a -> R b) -> R (a->b)
rlam f = R $ \g -> rec (repeat func) (gens g)
   where
     rec (h:hs) (j:js) = h j : rec hs js
     func g' x = head $ unR (f (R $ \_ -> [x])) g'

-- rlam' :: (R a -> R b) -> R (a->b)
-- rlam' f = R $ \g -> cycle [\x -> head $ unR (f (R $ \_ -> [x])) g]
rlam' f = R $ \g -> rec (repeat func) (gens g)
   where
     rec (h:hs) (j:js) = h j : rec hs js
     func g' x = unR (f (R $ const x)) g'


------------------------------------------------------------------------------
--
-- S interpreter
--
------------------------------------------------------------------------------

-- | \"S\" for showing patterns.
--
-- Enumeration for floating points are not working here also.
-- fromEnum and toEnum are assuming pval only.
newtype S s = S {unS :: forall a. (Show s) => a -> String}

-- | Show string representation of pattern.
showP :: (Show a) => S a -> String
showP p = unS p ()

instance (Show a, Eq a) => Eq (S a) where
  a == b = showP a == showP b

instance (Show a) => Show (S a) where
  show = showP

instance Typeable1 S where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.S") []

-- | Plain numbers would be shown as pval.
instance (Num a) => Num (S a) where
  a + b = S $ \_ -> showP a ++ " + " ++ showP b
  a * b = S $ \_ -> showP a ++ " * " ++ showP b
  abs n = S $ \_ -> "abs (" ++ showP n ++ ")"
  negate n = S $ \_ -> "negate (" ++ showP n ++ ")"
  signum n = S $ \_ -> "signum (" ++ showP n ++ ")"
  fromInteger n = S $ \_ -> "pval " ++ show (fromInteger n)

instance (Fractional a) => Fractional (S a) where
  a / b = S $ \_ -> showP a ++ " / " ++ showP b
  fromRational n = S $ \_ -> "pval " ++ show (fromRational n)

instance (Show a, Enum a) => Enum (S a) where
  pred n = S $ \_ -> "pred (" ++ showP n ++ ")"
  succ n = S $ \_ -> "succ (" ++ showP n ++ ")"
  fromEnum n = case words $ showP n of
    [x]         -> read x
    ["pval", x] -> fromEnum $ (read x :: Double) -- XXX: how to know the type?
    e           -> error $ "fromEnum: " ++ show e
  toEnum n = S $ \_ -> "pval " ++ show n

--
-- Instance for expressions
--

instance Pval S where
  pval a = S $ \_ -> "pval " ++ show a

instance Pempty S where
  pempty = S $ \_ -> "pempty"

instance Plist S where
  plist a = S $ \_ -> "plist " ++ show a

instance Pconcat S where
  pconcat ps = S $ \_ -> "pconcat " ++ showList ps ""

instance Pseq S where
  pseq n ps = S $ \_ -> "pseq (" ++ showP n ++ ") " ++ showList ps ""

instance Preplicate S where
  preplicate n p = S $ \_ -> "preplicate (" ++ showP n ++ ") (" ++ showP p

instance Prand S where
  prand n ps = S $ \x -> "prand (" ++ unS n x ++ ") " ++ showList ps ""

instance Prange S where
  prange lo hi = S $ \_ -> "prange (" ++ showP lo ++ ") (" ++ showP hi ++ ")"

instance Prandom S where
  prandom = S $ \_ -> "prandom"

instance Pchoose S where
  pchoose n ps = S $ \_ -> "pchoose (" ++ showP n ++ ") " ++ showList ps ""

instance Pcycle S where
  pcycle ps = S $ \x -> "pcycle " ++ showList ps ""

instance Prepeat S where
  prepeat a = S $ \_ -> "prepeat " ++ show a

instance Ploop S where
  ploop p = S $ \_ -> "ploop (" ++ show p ++ ")"

instance Papp S where
  papp a b = S $ \x -> "papp "

-- instance Plam S where
--   plam f = S $ \_ -> "\\x -> " ++ unS (f (S $ const "")) () ++ ")"

-- instance Papp S where
--   papp a b = S $ \x -> "(" ++ unS a x ++ " " ++ unS b x ++ ")"

--
-- More simple version of viewing patterns.
-- String representation of pattern /might/ take argument for showing variable.
--
-- Though still not sure /plam/ and /papp/ class would be made or not.
-- When I hit an idea for implementing R of lam and app, may remove this.
--
newtype V s = V {unV :: (Show s) => String}

viewP :: (Show a) => V a -> String
viewP = unV

instance (Show a) => Show (V a) where
  show (V a) = a

instance (Show a, Eq a) => Eq (V a) where
  V a == V b = a == b

instance Typeable1 V where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.V") []

instance (Num a) => Num (V a) where
  V a + V b = V $ a ++ "+" ++ b
  V a * V b = V $ a ++ "*" ++ b
  abs (V v) = V $ "abs (" ++ v ++ ")"
  negate (V n) = V $ "negate (" ++ n ++ ")"
  signum (V n) = V $ "signum (" ++ n ++ ")"
  fromInteger n = V $ "pval " ++ show n

instance (Fractional a) => Fractional (V a) where
  V a / V b = V $ a ++ " / " ++ b
  fromRational n = V $ "pval " ++ show (fromRational n)

instance (Show a, Enum a) => Enum (V a) where
  pred (V n) = V $ "pred (" ++ n ++ ")"
  succ (V n) = V $ "succ (" ++ n ++ ")"
  fromEnum (V n) = undefined
  toEnum n = V $ "pval " ++ show n

--
-- Instance definitions for expressions
--

instance Pval V where
  pval x = V $ "pval " ++ show x

instance Pempty V where
  pempty = V $ "pempty"

instance Plist V where
  plist xs = V $ "plist " ++ showList xs ""

instance Pseq V where
  pseq (V n) ps = V $ "pseq (" ++ n ++ ") " ++ showList ps ""

instance Prand V where
  prand (V n) ps = V $ "prand (" ++ n ++ ") " ++ showList ps ""

instance Prandom V where
  prandom  = V $ "prandom"

instance Prange V where
  prange (V lo) (V hi) = V $ "prange (" ++ lo ++ ") (" ++ hi ++ ")"

instance Pchoose V where
  pchoose (V n) ps = V $ "pchoose (" ++ n ++ ") " ++ showList ps ""

instance Pcycle V where
  pcycle ps = V $ "pcycle " ++ showList ps ""

instance Prepeat V where
  prepeat p = V $ "prepeat " ++ show p

instance Ploop V where
  ploop (V p) = V $ "ploop (" ++ p ++ ")"

------------------------------------------------------------------------------
--
-- Util
--
------------------------------------------------------------------------------

-- | Generates infinite list of RandomGen.
gens :: (RandomGen g) => g -> [g]
gens = iterate (snd . next)


------------------------------------------------------------------------------
--
-- Junks !!!
--
------------------------------------------------------------------------------

lam1 :: ((R a1 -> [a1]) -> [a]) -> R a
lam1 f = R $ \g -> f (\a -> unR a g)

lam2 f = R $ \g -> unR (f (\a -> unR a g)) g

foo :: (R a -> R b)
foo = undefined

foo' :: (StdGen -> [a]) -> StdGen -> [b]
foo' = unR . foo . R

foo'' f = unR . f . R

bar :: (R a -> R b) -> (StdGen -> [a]) -> (StdGen -> [b])
bar f = unR . f . R

bar' :: (R a -> R b) -> (StdGen -> [a]) -> R b
bar' f = R . unR . f . R

bar'' f = show . f . length

buzz g f = let h = flip (bar foo) g f in h

-- instance Plam V where
vlam f = V $ "plam " ++ unV (f (V ""))

class Ps p where
  ps :: (Show a, Show b, Show c) => p (a->b->c) -> p (a->b) -> p a -> p c

class Pk p where
  pk :: (Show b) => p a -> p b -> p a

instance Pk R where
  pk (R a) (R b) = R $ \g -> a g

instance Ps R where
  ps (R px) (R py) (R pz) = R $ \g -> f (px g) (py g) (pz g)
    where
      f :: [a -> b -> c] -> [a -> b] -> [a] -> [c]
      f (x:xs) (y:ys) (z:zs) = x z (y z) : f xs ys zs
      f _ _ _ = []

instance Pk S where
  pk a b = S $ const $ "pk (" ++ showP a ++ ") (" ++ showP b ++ ")"

k :: a -> b -> a
k a _ = a

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)
