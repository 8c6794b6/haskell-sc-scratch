{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

'R' pattern interpreter.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.R where


import Control.Applicative (Applicative(..), (<$>))
import Data.Data (Typeable1(..), mkTyCon, mkTyConApp)
import Prelude hiding
  ((++), (!!), length, concat, concatMap, reverse, head, take, tail
  ,replicate,zipWith, zipWith3, cycle, repeat, iterate, sum
  ,foldr1, Monad(..), (=<<), mapM_)
import System.Random (Random(..), RandomGen(..))

import Control.Monad.Stream
import Data.List.Stream
import System.Random.Mersenne.Pure64

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Prelude
import qualified Control.Parallel as CP

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
newtype R a = R {unR :: PureMT -> [a]}

-- | Run pattern with given seed.
runP :: R a -> PureMT -> [a]
runP = unR

-- | Run pattern with new seed.
runPIO :: R a -> IO [a]
runPIO p = unR p `fmap` newPureMT

mapPIO_ :: (a -> IO b) -> R a -> IO ()
mapPIO_ k p = mapM_ k =<< runPIO p

foldPIO :: (b -> a -> IO b) -> b -> R a -> IO b
foldPIO k z p = foldM k z =<< runPIO p

foldPIO_ :: (b -> a -> IO b) -> b -> R a -> IO ()
foldPIO_ k z p = foldPIO k z p >> return ()

instance (Eq a) => Eq (R a) where
  -- undefined !!
  (==) = undefined

instance (Show a) => Show (R a) where
  show _ = "R"

instance Functor R where
  fmap f (R r) = R $ \g -> fmap f (r g)

instance Monad R where
  return a = R $ \_ -> [a]
  (R r) >>= k = R $ \g ->
    let g' = snd . next $ g
    in  concatMap (\x -> unR (k x) g) (r g')

-- | Behaves same as ZipList.
instance Applicative R where
  pure x = R $ \_ -> repeat x
  R rf <*> R rv = R $ \g ->
    let g' = snd . next $ g
    in  zipWith id (rf g') (rv g)

instance Typeable1 R where
  typeOf1 _ = mkTyConApp (mkTyCon "Sound.SC3.Lepton.Pattern.Interpreter.R") []

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
  fromEnum (R r)  = fromEnum $ head (r (undefined :: PureMT))
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
  pappend pa pb = R $ \g ->
    let g' = snd . next $ g
    in  unR pa g ++ unR pb g'

-- | Repeats the list of pattern with given number.
instance Pseq R where
  -- pseq n ps = R $ \g ->
  --   let ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
  --   in  concat $ zipWith unR ps' (gens g)
  pseq n = preplicate n . foldr1 pappend

-- | Replicate given pattern for given time.
instance Preplicate R where
  preplicate n p = R $ \g ->
    let p' = concatMap (`replicate` p) (unR n g)
    in  concat $ zipWith unR p' (gens g)

-- | Choose element from given list for number of given times.
instance Prand R where
  prand n p = R $ \g ->
    let g' = snd . next $ g
        gs = take (sum $ unR n g) (gens g')
    in  concatMap (\h -> let (j,_) = randomR (0,length p - 1) h
                         in  unR (p!!j) h) gs

-- lo and hi bounds won't vary with: randomRs (lo, hi) g
instance Prange R where
  prange lo hi =  R $ \g0 ->
    let g1 = snd . next $ g0
        g2 = snd . next $ g1
    in  zipWith3 (\l h g' -> fst $ randomR (l,h) g')
          (unR lo g0) (unR hi g1) (gens g2)

instance Prandom R where
  prandom = R randoms

instance Pchoose R where
  pchoose = prand

instance Pcycle R where
  -- pcycle ps = R $ \g -> concat $ zipWith unR (cycle ps) (gens g)
  pcycle ps = pforever (pseq 1 ps)

instance Prepeat R where
  prepeat a = R $ \_ -> repeat a

instance Pforever R where
  pforever p = R $ \g -> concatMap (unR p) (gens g)

instance Pshuffle R where
  pshuffle ps = R $ \g ->
    let g' = snd . next $ g
    in  concat $ zipWith unR (shuffle ps g) (gens g')

instance Pmerge R where
  pmerge = merge

instance Ppar R where
  ppar = foldr1 pmerge

instance (Ord a, Num a) => Mergable (R (ToOSC a)) where
  merge p1 p2 = R $ \g ->
    let p1' = unR p1 g
        p2' = unR p2 g
    in  p1' `CP.par` (p2' `CP.pseq` merge p1' p2')

instance (Ord a, Num a) => Mergable [ToOSC a] where
  merge a b = mergeL 0 (0,0) a b

mergeL ::
  (Ord a, Num a) =>
  a -> (a, a) -> [ToOSC a] -> [ToOSC a] -> [ToOSC a]
mergeL _ _ [] [] = []
mergeL t (ta,_) (a:as) [] = tadjust "dur" (const $ getDur a + ta - t) a : as
mergeL t (_,tb) [] (b:bs) = tadjust "dur" (const $ getDur b + tb - t) b : bs
mergeL t (ta,tb) (a:as) (b:bs)
  | ua <= ub  = tadjust "dur" (const $ ua-t) a : mergeL ua (ua,tb) as (b:bs)
  | otherwise = tadjust "dur" (const $ ub-t) b : mergeL ub (ta,ub) (a:as) bs
  where
    ua = ta + getDur a
    ub = tb + getDur b

-- | Same as @(\<*\>)@.
instance Papp R where
  papp = (<*>)

instance Plam R where
  plam = rlam

rlam :: (R a -> R b) -> R (a->b)
rlam f = R $ \g -> rec (repeat func) (gens g)
   where
     rec (h:hs) (j:js) = h j : rec hs js
     rec _ _ = []
     func g' x = head $ unR (f (R $ \_ -> [x])) g'

-- rlam' :: (R a -> R b) -> R (a->b)
-- rlam' f = R $ \g -> cycle [\x -> head $ unR (f (R $ \_ -> [x])) g]
-- rlam' f = R $ \g -> rec (repeat func) (gens g)
--    where
--      rec (h:hs) (j:js) = h j : rec hs js
--      func g' x = unR (f (R $ const x)) g'

------------------------------------------------------------------------------
--
-- Util
--
------------------------------------------------------------------------------

-- | Generates infinite list of RandomGen.
-- gens :: (RandomGen g) => g -> [g]
gens :: PureMT -> [PureMT]
gens = iterate (snd . next)

-- | Shuffle the elements in list, inspired from 'perfect random shuffle' by
-- Oleg.
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle xs g = shuffle1 xs $ fst $ make_rs (Prelude.length xs - 1) g

data Tree a = Leaf a
            | Node !Int (Tree a) (Tree a)
            deriving (Show)

build_tree :: [a] -> Tree a
build_tree = grow_level . Prelude.map Leaf
  where
    grow_level [node] = node
    grow_level l = grow_level $ inner l

    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = joinTree e1 e2 : inner rest

    joinTree l r = case (l,r) of
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
    shuffle1' _ _ = []

    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf e)) k = e:k l
    extract_tree n (Node c l@Leaf{} r) k =
      extract_tree (n-1) r (k . Node (c-1) l)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
    extract_tree n (Node c l@(Node cl _ _) r) k
      | n < cl    = extract_tree n l (\l' -> k $ Node (c-1) l' r)
      | otherwise = extract_tree (n-cl) r (k . Node (c-1) l)
    extract_tree _ _ _ = []

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g0 = loop [] n g0
  where
    loop acc 0 g = (reverse acc, g)
    loop acc m g = loop (r:acc) (pred m) g'
      where (r,g') = randomR (0,n) g
