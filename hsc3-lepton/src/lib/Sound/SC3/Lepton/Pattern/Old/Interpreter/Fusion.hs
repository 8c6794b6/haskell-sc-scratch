{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Fusion pattern interpreter.

TODO:

* 'pcycle' is not returning value when merged. Fix this.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.Fusion where

import Control.Applicative hiding (empty)
import Control.Monad (liftM)
import Data.Data
import System.Random
import Prelude hiding
  (length, null, (++), drop, head, last, (!!), init, tail, take
  ,foldr, foldr1, foldl, foldl1, map, mapM, mapM_, concatMap, concat
  ,replicate, repeat, sum ,zipWith, zipWith3, cycle )

import Data.Vector.Fusion.Stream
import Data.Vector.Fusion.Stream.Size
import System.Random.Mersenne.Pure64
import System.Random.Shuffle (shuffle')

import qualified Prelude

import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Stream.Size as SZ

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

-- | \"F\" for running pattern and achieving 'Stream'.
newtype R a = R {unR :: RandomGen g => g -> Stream a}
{-# INLINEABLE unR #-}
{-# SPECIALISE unR :: PureMT -> Stream a #-}

instance F.Foldable Stream where
  foldr = foldr
  foldl = foldl
  foldr1 = foldr1
  foldl1 = foldl1

instance Eq a => Eq (R a) where
  R a == R b = a g `eq` b g where g = pureMT 0

instance Functor R where
  fmap f (R v) = R $ \g -> fmap f (v g)

instance Applicative R where
  pure = prepeat
  R f <*> R v = R $ \g -> f g <*> v g

instance Monad R where
  return = pval
  R a >>= k = R $ \g ->
    let g' = snd (next g)
    in  concatMap (\x -> unR (k x) g) (a g')

instance Num a => Num (R a) where
  a + b = (+) <$> a <*> b
  a * b = (*) <$> a <*> b
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pval . fromInteger

instance Fractional a => Fractional (R a) where
  a / b = (/) <$> a <*> b
  fromRational = pure . fromRational

instance (Enum a) => Enum (R a) where
  succ = fmap succ
  pred = fmap pred
  fromEnum (R r)  = fromEnum $ head (r (undefined::PureMT))
  toEnum = pure . toEnum

instance Typeable1 R where
  typeOf1 _ = mkTyConApp (mkTyCon n) []
    where n = "Sound.SC3.Lepton.Pattern.Interpreter.Fusion"

runP :: RandomGen g => R a -> g -> [a]
runP p g = toList $ unR p g

runPIO :: R a -> IO (Stream a)
runPIO p = unR p <$> newPureMT

foldPIO :: (a -> b -> IO a) -> a -> R b -> IO a
foldPIO k z p = runPIO p >>= \p' -> foldM' k z p'

foldPIO_ :: (a -> b -> IO a) -> a -> R b -> IO ()
foldPIO_ k z p = foldPIO k z p >> return ()

mapPIO_ :: (a -> IO b) -> R a -> IO ()
mapPIO_ k p = runPIO p >>= \p' -> mapM_ k p'

instance Show a => Show (R a) where
  show _ = "R"

instance Pval R where
  pval a = R $ \_ -> singleton a

instance Pempty R where
  pempty = R $ const empty

instance Plist R where
  plist as = R $ \_ -> fromList as

instance Pconcat R where
  pconcat ps = R $ \g -> concat $ zipWith unR (fromList ps) (gens g)

instance Pappend R where
  pappend a b = R $ \g ->
    let g' = snd . next $ g
    in  unR a g ++ unR b g'

instance Pseq R where
  -- pseq n ps = R $ \g ->
  --   let n' = sum (unR n g)
  --       ps' = concatMap (\n' -> concat $ replicate n' (fromList ps)) (unR n g)
  --   in  concat $ zipWith unR ps' (gens g)
  pseq n ps = preplicate n (Prelude.foldr1 pappend ps)

instance Preplicate R where
  preplicate n p = R $ \g ->
    let p' = concatMap (`replicate` p) (unR n g)
    in  concat $ zipWith unR p' (gens g)

instance Prange R where
  prange lo hi = R $ \g0 ->
    let g1 = snd (next g0)
        g2 = snd (next g1)
    in  zipWith3 (\l h g -> fst $ randomR (l,h) g)
        (unR lo g0) (unR hi g1) (gens g2)

instance Prand R where
  prand n p = R $ \g ->
    let g' = snd (next g)
        gs = take (sum $ unR n g) (gens g')
        p' = fromList p
    in  concatMap (\h -> let (j,_) = randomR (0,length p' - 1) h
                         in  unR (p' !! j) h) gs

instance Pchoose R where
  pchoose = prand

instance Prandom R where
  prandom = R $ \g -> fromList (randoms g)

instance Prepeat R where
  prepeat a = R $ \_ -> repeat a

instance Pforever R where
  pforever p = R $ \g -> concatMap (unR p) (gens g)

instance Pcycle R where
  -- pcycle ps = R $ \g -> concat $ zipWith unR (cycle (fromList ps)) (gens g)
  -- pcycle ps = R $ \g -> concatMap (zipWith . unR) (cycle (fromList ps)) (gens g)
  -- pcycle ps = R $ \g ->
  --   cycle $ concat $ zipWith unR (fromList ps) (gens g)
  pcycle ps = pforever (pconcat ps)

instance Pshuffle R where
  pshuffle ps = R $ \g ->
    let g' = snd . next $ g
    in  concat $ zipWith unR
        (fromList $ shuffle' ps (Prelude.length ps) g) (gens g')

instance (Ord a, Num a) => Mergable (Stream (ToOSC a)) where
  merge = mergeS 0 (0,0)

instance (Ord a, Num a) => Mergable (R (ToOSC a)) where
  merge p1 p2 = R $ \g ->
    let p1' = unR p1 g
        p2' = unR p2 g
    in  merge p1' p2'

instance Pmerge R where
  pmerge = merge

instance Ppar R where
  ppar = Prelude.foldr1 pmerge

mergeS ::
  (Ord a, Num a) =>
  a -> (a, a) -> Stream (ToOSC a) -> Stream (ToOSC a) -> Stream (ToOSC a)
mergeS t (ta,tb) ass bss
  | null ass && null bss = empty
  | null bss  = tadjust "dur" (const $ ua-t) a `cons` as
  | null ass  = tadjust "dur" (const $ ub-t) b `cons` bs
  | ua <= ub  = tadjust "dur" (const $ ua - t) a `cons` mergeS ua (ua,tb) as bss
  | otherwise = tadjust "dur" (const $ ub - t) b `cons` mergeS ub (ta,ub) bs ass
  where
    as = if null ass then empty else tail ass
    bs = if null bss then empty else tail bss
    a = head ass
    b = head bss
    ua = ta + getDur a
    ub = tb + getDur b

-- mergeT :: (Ord a, Num a) =>
--   a -> (a, a) -> Stream (ToOSC a) -> Stream (ToOSC a) -> Stream (ToOSC a)
-- mergeT t (ta,tb) sa@(SM.Stream stepA sdA szA) sb@(SM.Stream stepB sdB szB)
--   | null sa && null sb = empty
--   | null sa =
--     let a

-- ---------------------------------------------------------------------------
-- Utils

-- | Behaves similar to 'ZipList'
instance Monad m => Applicative (SM.Stream m) where
  pure x = SM.Stream step x Unknown where
    step _ = return (Yield x x)
  SM.Stream stepr sr zr <*> SM.Stream stepl sl zl =
    SM.Stream step' (sr,sl) (SZ.smaller zr zl)
    where
      step' (sa,sb) = do
        r <- stepr sa
        l <- stepl sb
        case (r,l) of
          (Yield f fs, Yield y ys) -> return (Yield (f y) (fs,ys))
          (Skip  fs  , Yield _ _)  -> step' (fs,sb)
          (Yield _ _ , Skip ys)    -> step' (sa,ys)
          (Skip xs   , Skip ys)    -> step' (xs,ys)
          _                        -> return Done

sum :: Num a => Stream a -> a
sum = foldr (+) 0

cycle :: Stream a -> Stream a
cycle (SM.Stream step s0 _) = SM.Stream step' s0 Unknown where
  step' s = do
    r <- step s
    case r of
      Yield x s' -> return (Yield x s')
      Skip s'    -> step' s'
      _          -> step' s0

{-
cycle xs = xs ++ cycle xs
-}

scycle :: Stream a -> Stream a
scycle = cycle

concat :: Stream (Stream a) -> Stream a
concat = concatMap id

{-
concat = foldr1' (++)
-}

repeat :: a -> Stream a
repeat x = SM.Stream step x Unknown where
  step _ = return $ Yield x x

{-
repeat x = unfoldr (\_ -> Just (x,undefined)) x
-}

instance T.Traversable Stream where
  traverse f = foldr cons' (pure empty) where
    cons' x xs = cons <$> f x <*> xs
  -- sequenceA = foldr f (pure empty)
  --   where f v acc = cons <$> v <*> acc

gens :: RandomGen g => g -> Stream g
gens = unfoldr $ \g -> let (_,g') = next g in Just (g',g')

{-

do { r <- return $ sum [1..10]
   ; putStrLn $ "sum is: " ++ show r }

do { putStrLn "hello"
   ; putStrLn "emacs" }

-}
