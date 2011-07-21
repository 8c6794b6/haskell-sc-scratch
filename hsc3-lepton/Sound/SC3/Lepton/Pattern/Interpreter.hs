{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Control.Category (Category(..))
import Control.Monad (Functor(..), Monad(..))
import Data.Data (Typeable1(..), mkTyCon, mkTyConApp)
import Data.Map (Map)
import Prelude
  ( Double, Enum(..), Eq(..), Fractional(..), Num(..), Show(..), IO, String, Int
  , ($), (<), const, error, flip, fst, otherwise, read, snd, undefined)
import System.Random (StdGen, Random(..), RandomGen, next, newStdGen)
import qualified Data.Map as M

import Data.List.Stream

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
runP = unR

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

instance Pappend R where
  pappend pa pb = R $ \g -> unR pa g ++ unR pb g

-- | Repeats the list of pattern with given number.
instance Pseq R where
  pseq n ps = R $ \g ->
    let ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
    in  concat $ zipWith unR ps' (gens g)

-- | Replicate given pattern for given time.
instance Preplicate R where
  preplicate n p = R $ \g ->
    let ps = concatMap (`replicate` p) (unR n g)
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

instance Pforever R where
  pforever p = R $ \g -> concatMap (unR p) (gens g)

instance Pshuffle R where
  pshuffle ps = R $ \g -> concat $ zipWith unR (shuffle ps g) (gens g)

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
    ["pval", x] -> fromEnum (read x :: Double) -- XXX: how to tell the type?
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

instance Pappend S where
  pappend a b = S $ \_ -> "pappend (" ++ showP a ++ ") (" ++ showP b ++ ")"

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

instance Pshuffle S where
  pshuffle ps = S $ \_ -> "pshuffle " ++ showList ps ""

instance Pchoose S where
  pchoose n ps = S $ \_ -> "pchoose (" ++ showP n ++ ") " ++ showList ps ""

instance Pcycle S where
  pcycle ps = S $ \x -> "pcycle " ++ showList ps ""

instance Prepeat S where
  prepeat a = S $ \_ -> "prepeat " ++ show a

instance Pforever S where
  pforever p = S $ \_ -> "pforever (" ++ show p ++ ")"

instance Papp S where
  papp a b = S $ \_ -> "papp "

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
  pempty = V "pempty"

instance Plist V where
  plist xs = V $ "plist " ++ showList xs ""

instance Pseq V where
  pseq (V n) ps = V $ "pseq (" ++ n ++ ") " ++ showList ps ""

instance Prand V where
  prand (V n) ps = V $ "prand (" ++ n ++ ") " ++ showList ps ""

instance Prandom V where
  prandom = V "prandom"

instance Pshuffle V where
  pshuffle ps = V $ "pshuffle " ++ showList ps ""

instance Prange V where
  prange (V lo) (V hi) = V $ "prange (" ++ lo ++ ") (" ++ hi ++ ")"

instance Pchoose V where
  pchoose (V n) ps = V $ "pchoose (" ++ n ++ ") " ++ showList ps ""

instance Pcycle V where
  pcycle ps = V $ "pcycle " ++ showList ps ""

instance Prepeat V where
  prepeat p = V $ "prepeat " ++ show p

instance Pforever V where
  pforever (V p) = V $ "pforever (" ++ p ++ ")"

------------------------------------------------------------------------------
--
-- Util
--
------------------------------------------------------------------------------

-- | Generates infinite list of RandomGen.
gens :: (RandomGen g) => g -> [g]
gens = iterate (snd . next)

-- | Shuffle the elements in list, inspired from 'perfect random shuffle' by
-- Oleg.
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle xs g = shuffle1 xs $ fst $ make_rs (length xs - 1) g

data Tree a = Leaf a
            | Node !Int (Tree a) (Tree a)
            deriving (Show)

build_tree = grow_level . map Leaf
  where
    grow_level [node] = node
    grow_level l = grow_level $ inner l

    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = join e1 e2 : inner rest

    join l r = case (l,r) of
      (Leaf _,Leaf _)             -> Node 2 l r
      (Node ct _ _,Leaf _)        -> Node (ct+1) l r
      (Leaf _,Node ct _ _)        -> Node (ct+1) l r
      (Node ctl _ _,Node ctr _ _) -> Node (ctl+ctr) l r

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elems rseq = shuffle1' (build_tree elems) rseq
  where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) =
      extract_tree ri tree (`shuffle1'` r_others)

    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf e)) k = e:k l
    extract_tree n (Node c l@Leaf{} r) k =
      extract_tree (n-1) r (k . Node (c-1) l)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
    extract_tree n (Node c l@(Node cl _ _) r) k
      | n < cl    = extract_tree n l (\l' -> k $ Node (c-1) l' r)
      | otherwise = extract_tree (n-cl) r (k . Node (c-1) l)

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
    loop acc 0 g = (reverse acc, g)
    loop acc n g = loop (r:acc) (pred n) g'
      where (r,g') = randomR (0,n) g



------------------------------------------------------------------------------
--
-- Junks !!!
--
------------------------------------------------------------------------------

lam1 :: ((R a1 -> [a1]) -> [a]) -> R a
lam1 f = R $ \g -> f (`unR` g)

lam2 f = R $ \g -> unR (f (`unR` g)) g

foo :: (R a -> R b)
foo = undefined

foo' :: (StdGen -> [a]) -> StdGen -> [b]
foo' = unR . foo . R

foo'' f = unR . f . R

bar :: (R a -> R b) -> (StdGen -> [a]) -> StdGen -> [b]
bar f = unR . f . R

bar' :: (R a -> R b) -> (StdGen -> [a]) -> R b
bar' f = R . unR . f . R

bar'' f = show . f . length

buzz g f = let h = (bar foo f g) in h

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
