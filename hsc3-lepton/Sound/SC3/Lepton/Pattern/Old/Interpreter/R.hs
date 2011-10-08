{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (FlexibleInstances)

'R' pattern interpreter.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.R
  ( -- * Guts
    R(..)
  , toR
  , runP
  , runPIO
  , mapPIO_
  , foldPIO
  , foldPIO_
    -- * Utils
  , nan
  , gens
  , initialT
  , shiftT
  , mergeL
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Data
  (Typeable(..), Typeable1(..), mkTyCon, mkTyConApp, typeOfDefault)
import Prelude hiding
  ((++), (!!), length, concat, concatMap, reverse, head, take, tail
  ,replicate,zipWith, zipWith3, cycle, repeat, iterate, sum
  ,foldr1, Monad(..), (=<<), map, mapM_, zip)
import System.Random (Random(..), RandomGen(..))

import Control.Monad.Stream
import Data.Binary (Binary)
import Data.List.Stream
import Sound.SC3 hiding (Binary)
import System.Random.Mersenne.Pure64
import System.Random.Shuffle (shuffle')

import Sound.SC3.Lepton.Pattern.Expression
import Sound.SC3.Lepton.Pattern.ToOSC

import qualified Prelude
import qualified Control.Parallel as CP
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Traversable as T

-- | \"R\" for running patterns.
--
-- Note that enumeration for floating point values are not working,
--
-- e.g: When getting single random element from @[0.1,0.2 .. 2.0]@, below will
-- not work:
--
-- > runPIO $ prand 1 [0.1,0.2..2.0]
--
-- In above case, explicitly use pattern language to make desired value:
--
-- > runPIO $ prand 1 $ map pval [0.1,0.2..1.0]
--
newtype R a = R {unR :: PureMT -> [a]}

-- | Alias function of 'id' for fixing type.
toR :: R a -> R a
toR = id

-- | Run pattern with given seed.
runP :: R a -> PureMT -> [a]
runP = unR

-- | Run pattern with new seed.
runPIO :: R a -> IO [a]
runPIO p = unR p `fmap` newPureMT

-- | Apply given IO action to pattern.
mapPIO_ :: (a -> IO b) -> R a -> IO ()
mapPIO_ k p = mapM_ k =<< runPIO p

-- | Fold pattern with given IO action.
foldPIO :: (b -> a -> IO b) -> b -> R a -> IO b
foldPIO k z p = foldM k z =<< runPIO p

-- | Fold pattern with given IO action, discarding result.
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

instance Typeable a => Typeable (R a) where
  typeOf = typeOfDefault

instance (Num a) => Num (R a) where
  a + b = (+) <$> a <*> b
  a * b = (*) <$> a <*> b
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

instance (Fractional a) => Fractional (R a) where
  a / b = (/) <$> a <*> b
  recip = fmap recip
  fromRational = return . fromRational

instance Floating a => Floating (R a) where
  pi = return pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) a b = (**) <$> a <*> b
  logBase a b = logBase <$> a <*> b
  sin = fmap sin
  tan = fmap tan
  cos = fmap cos
  asin = fmap asin
  atan = fmap atan
  acos = fmap acos
  sinh = fmap sinh
  tanh = fmap tanh
  cosh = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance UnaryOp a => UnaryOp (R a) where
  ampDb = fmap ampDb
  asFloat = fmap asFloat
  asInt = fmap asInt
  bitNot = fmap bitNot
  cpsMIDI = fmap cpsMIDI
  cpsOct = fmap cpsOct
  cubed = fmap cubed
  dbAmp = fmap dbAmp
  distort = fmap distort
  frac = fmap frac
  isNil = fmap isNil
  log10 = fmap log10
  log2 = fmap log2
  midiCPS = fmap midiCPS
  midiRatio = fmap midiRatio
  notE = fmap notE
  notNil = fmap notNil
  octCPS = fmap octCPS
  ramp_ = fmap ramp_
  ratioMIDI = fmap ratioMIDI
  softClip = fmap softClip
  squared = fmap squared

instance Ord a => Ord (R a) where
  compare a b = head $ unR (compare <$> a <*> b) (pureMT 0)

instance (Enum a) => Enum (R a) where
  succ = fmap succ
  pred = fmap pred
  fromEnum (R r)  = fromEnum $ head (r (undefined :: PureMT))
  toEnum = return . toEnum

------------------------------------------------------------------------------
-- Instance for pattern expressions

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
  pcycle [] = R $ const []
  pcycle ps = pforever (pseq 1 ps)

instance Prepeat R where
  prepeat a = R $ \_ -> repeat a

instance Pforever R where
  pforever p = R $ \g -> concatMap (unR p) (gens g)

instance Pshuffle R where
  pshuffle ps = R $ \g ->
    let g' = snd . next $ g
    in  concat $ zipWith unR (shuffle' ps (length ps) g) (gens g')

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

instance Mergable (R Double) where
  -- XXX: dummy
  merge = undefined

instance Mergable (R Int) where
  -- XXX: dummy
  merge = undefined

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

instance PtakeT R where
  ptakeT t p = R $ \g ->
    let ps = runP p g
        f cur rs = case rs of
          [] -> []
          (q:qs) | cur <= t  -> q : f (cur+getDur q) qs
                 | otherwise -> []
    in  f 0 ps

instance PdropT R where
  pdropT t p = R $ \g ->
    let ps = runP p g
        f cur rs = case rs of
          []                 -> []
          (q:qs) | cur > t   -> rs
                 | otherwise -> f (cur+getDur q) qs
    in  f 0 ps

-- | First argument is choice for initial index, and second is
-- a list of paif of pattern and list of index for next choice.
instance Pfsm R where
  pfsm is cs = R $ \g0 -> go (head $ shuffle' is (length is) g0) g0 where
    cm = I.fromList (zip [0..] cs)
    go idx g = case I.lookup idx cm of
      Nothing     -> []
      Just (p,[]) -> runP p g
      Just (p,js) ->
        let g'   = snd (next g)
            idx' = head $ shuffle' js (length js) g
        in  runP p g ++ go idx' g'

instance Psnew R where psnew = mkSnew

instance Pnset R where pnset = mkNset

-- | Make 's_new' messages.
mkSnew ::
  (Binary a, Num a) =>
  String -> Maybe Int -> AddAction -> Int -> [(String, R a)] -> R (ToOSC a)
mkSnew def nid aa tid ms = ToOSC sn <$> ms' where
  sn = Snew def nid aa tid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g
{-# INLINE mkSnew #-}
{-# SPECIALISE mkSnew ::
    String -> Maybe Int -> AddAction -> Int
    -> [(String, R Double)] -> R (ToOSC Double) #-}

-- | Make 'n_set' messages.
mkNset :: (Binary a, Num a) => Int -> [(String, R a)] -> R (ToOSC a)
mkNset nid ms = ToOSC o <$> ms' where
  o = Nset nid
  ms' = R $ \g ->
    tail $ shiftT 0 $
    unR (pappend (pval initialT) (T.sequenceA $ M.fromList ms)) g
{-# INLINE mkNset #-}
{-# SPECIALISE mkNset :: Int -> [(String,R Double)] -> R (ToOSC Double) #-}

initialT :: Num a => M.Map String a
initialT = M.singleton "dur" 0
{-# INLINE initialT #-}
{-# SPECIALIZE initialT :: M.Map String Double #-}

shiftT :: Num a => a -> [M.Map String a] -> [M.Map String a]
shiftT t ms = case ms of
  (m1:m2:r) ->
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  (m1'):shiftT t' (m2:r)
  [m1] ->
    -- XXX:
    -- Sending dummy silent event at the end of list
    -- to receive /n_go response from server.
    --
    let m1' = M.adjust (const t) "dur" m1
        t'  = M.findWithDefault 0 "dur" m1
    in  [m1',M.fromList [("freq",0),("dur",t')]]
  _ -> []
{-# INLINE shiftT #-}

instance Plam R where
  plam k = R $ \_ -> repeat (\x -> (k (R $ const [x])))

instance Papp R where
  papp k p = R $ \g -> unR (pconcat $ zipWith ($) (unR k g) (unR p g)) g

------------------------------------------------------------------------------
-- Util

-- | Generates infinite list of RandomGen.
-- gens :: (RandomGen g) => g -> [g]
gens :: PureMT -> [PureMT]
gens = iterate (snd . next)

-- | 'NaN' value made by @0/0@.
nan :: Floating a => a
nan = 0/0
{-# SPECIALIZE nan :: Double #-}
