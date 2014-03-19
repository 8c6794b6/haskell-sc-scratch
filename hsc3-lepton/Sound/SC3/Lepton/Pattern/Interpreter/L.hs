{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

L Instances of classes in PC02, using System.Random.MWC.

Couple instances are using unsafe function.

-}
module Sound.SC3.Lepton.Pattern.Interpreter.L
  ( L(..), toL
  , runLIO
  , mapLIO_
  , foldLIO
  , foldLIO_
  ) where

import Control.Applicative
import Control.Monad
import System.IO.Unsafe

import Control.Monad.Primitive
import Control.Monad.IO.Class (MonadIO(..))
import Sound.SC3
import System.Random.MWC
import System.Random.Shuffle

import Sound.SC3.Lepton.Pattern.ToOSC
import Sound.SC3.Lepton.Pattern.Expression.Class

import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified System.Random as R

------------------------------------------------------------------------------
-- L, with MWC random.

-- | Newtype for running pattern and make lists with using System.Random.MWC.
newtype L h a = L {unL :: h -> (Gen (PrimState IO)) -> IO [a]}

toL :: L () a -> L () a
toL = id

runLIO :: MonadIO m => L () a -> m [a]
runLIO l = liftIO $ withSystemRandom (unL l ())

mapLIO_ :: MonadIO m => (a -> m ()) -> L () a -> m ()
mapLIO_ k l = mapM_ k =<< runLIO l

foldLIO :: MonadIO m => (b -> a -> m b) -> b -> L () a -> m b
foldLIO k z l = foldM k z =<< runLIO l

foldLIO_ :: MonadIO m => (b -> a -> m b) -> b -> L () a -> m ()
foldLIO_ k z l = foldLIO k z l >> return ()

------------------------------------------------------------------------------
-- Helpers

constL2 :: a -> L h a
constL2 x = L $ \_ _ -> return [x]
{-# INLINE constL2 #-}

liftL2 :: ([a] -> [b] -> [c]) -> L h a -> L h b -> L h c
liftL2 f a b = L $ \h g -> liftA2 f (unL a h g) (unL b h g)
{-# INLINE liftL2 #-}

rangeL :: Variate a => L h a -> L h a -> L h a
rangeL lo hi = L $ \h g -> do
  los <- unL lo h g
  his <- unL hi h g
  zipWithM uniformR (zip los his) (repeat g)
{-# INLINE rangeL #-}

predTL ::
  (Double -> Double -> Bool)
  -> L h Double -> L h (ToOSC Double) -> L h (ToOSC Double)
predTL cond t p = L $ \h g -> do
    p' <- unL p h g
    t' <- head <$> unL t h g
    let f cur xs = case xs of
          [] -> []
          (y:ys) | cond t' cur -> y : f (cur+getDur y) ys
                 | otherwise   -> []
    return $ f 0 p'
{-# INLINE predTL #-}

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

initialT :: Num a => M.Map String a
initialT = M.singleton "dur" 0
{-# INLINE initialT #-}

mergeL :: Double -> (Double,Double) ->
          [ToOSC Double] -> [ToOSC Double] -> [ToOSC Double]
mergeL _ _ [] [] = []
mergeL t (ta,_) (a:as) [] = tadjust "dur" (const $ getDur a + ta - t) a : as
mergeL t (_,tb) [] (b:bs) = tadjust "dur" (const $ getDur b + tb - t) b : bs
mergeL t (ta,tb) (a:as) (b:bs)
  | ua <= ub  = tadjust "dur" (const $ ua-t) a : mergeL ua (ua,tb) as (b:bs)
  | otherwise = tadjust "dur" (const $ ub-t) b : mergeL ub (ta,ub) (a:as) bs
  where
    ua = ta + getDur a
    ub = tb + getDur b
{-# INLINE mergeL #-}

------------------------------------------------------------------------------
-- Base classes

instance Functor (L h) where
  fmap f (L l) = L $ \h g -> l h g >>= return . map f

instance Applicative (L h) where
  pure x = L $ \_ _ -> return $ repeat x
  L a <*> L b = L $ \h g -> liftM2 (zipWith ($)) (a h g) (b h g)

instance Monad (L h) where
  return x = L $ \_ _ -> return [x]
  l >>= k = L $ \h g -> do
    ls <- unL l h g
    unL (pconcat (map k ls)) h g

instance Show (L h a) where
  show _ = "L"

instance Eq (L h a) where
  _ == _ = True

instance Num a => Num (L h a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger x = L $ \_ _ -> return [fromInteger x]

instance Fractional a => Fractional (L h a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational x = L $ \_ _ -> return [fromRational x]

------------------------------------------------------------------------------
-- Patterns

instance Pint L where
  pint = constL2
  (+!) = (+)
  (*!) = (*)
  (-!) = (-)
  piabs = abs
  pinegate = negate
  pisignum = signum
  pirange = rangeL

instance Pdouble L where
  pdouble = constL2
  (+@) = (+)
  (*@) = (*)
  (-@) = (-)
  pdabs = fmap abs
  pdnegate = fmap negate
  pdsignum = fmap signum
  pdrange = rangeL
  (/@) = liftA2 (/)
  precip = fmap recip
  ppi = pure pi
  pexp = fmap exp
  psqrt = fmap sqrt
  plog = fmap log
  (**@) = liftA2 (**)
  plogBase = liftA2 logBase
  psin = fmap sin
  ptan = fmap tan
  pcos = fmap cos
  pasin = fmap asin
  patan = fmap atan
  pacos = fmap acos
  psinh = fmap sinh
  ptanh = fmap tanh
  pcosh = fmap cosh
  pasinh = fmap asinh
  patanh = fmap atanh
  pacosh = fmap acosh
  pampDb = fmap ampDb
  pasFloat = fmap asFloat
  pasInt = fmap asInt
  -- pbitNot = fmap bitNot
  pcpsMIDI = fmap cpsMIDI
  pcpsOct = fmap cpsOct
  pcubed = fmap cubed
  pdbAmp = fmap dbAmp
  pdistort = fmap distort
  pfrac = fmap frac
  pisNil = fmap isNil
  plog10 = fmap log10
  plog2 = fmap log2
  pmidiCPS = fmap midiCPS
  pmidiRatio = fmap midiRatio
  pnotE = fmap notE
  pnotNil = fmap notNil
  poctCPS = fmap octCPS
  pramp_ = fmap ramp_
  pratioMIDI = fmap ratioMIDI
  psoftClip = fmap softClip
  psquared = fmap squared

instance Pappend L where
  pappend = liftL2 (++)

instance Pconcat L where
  pconcat = foldr1 pappend

instance Preplicate L where
  preplicate n p = L $ \h g -> do
    n' <- unL n h g
    concat <$> mapM (\p' -> unL p' h g) (replicate (sum n') p)

instance Pseq L where
  pseq n = preplicate n . pconcat

-- XXX: Unsafe.
instance Pforever L where
  pforever p = L $ \h g ->
    let go = do
          xs <- unL p h g
          (xs ++) `liftM` (unsafeInterleaveIO go)
    in  go

instance Pcycle L where
  pcycle = pforever . pconcat

instance Prand L where
  prand n es = L $ \h g0 -> do
    num <- sum `liftM` (unL n h g0)
    let go g' = do
          i <- uniformR (0,length es-1) g'
          unL (es!!i) h g'
    concat `liftM` replicateM num (go g0)

instance Pshuffle L where
  pshuffle ls = L $ \h g0 -> do
    ls' <- pconcat <$> (shuffle' ls (length ls) <$> R.newStdGen)
    unL ls' h g0

instance Ptuple L where
  pzip = liftL2 zip
  pfst = fmap fst
  psnd = fmap snd

-- XXX: Using 'unsafeInterleaveIO'.
instance Pfsm L where
  pfsm is ls = L $ \h g -> do
    ini <- uniformR (0,length is-1) g
    let cm = I.fromList (zip [0..] ls)
        go idx = case I.lookup idx cm of
          Nothing -> return []
          Just (p,[]) -> unL p h g
          Just (p,js) -> do
            idx' <- uniformR (0,length js) g
            xs <- unL p h g
            (xs ++) `liftM` unsafeInterleaveIO (go idx')
    go ini

-- XXX: Using 'unsafePerformIO' in plam
instance Plambda L where
  pz = L $ \(a,_) _ -> return [a]
  ps v = L $ \(_,h) g -> unL v h g
  plam _ k = L $ \h g ->
    return $ repeat (\x -> unsafePerformIO $ unL k (x,h) g)
  papp e1 e2 = L $ \h g -> do
    e1' <- unL e1 h g
    e2' <- unL e2 h g
    return $ concat $ zipWith ($) e1' e2'

instance Psnew L where
  psnew def nid aa tid ms = L $ \h g -> do
    let sn = Snew def nid aa tid
    ms' <- unL (T.sequenceA $ M.fromList ms) h g
    return $ fmap (ToOSC sn) (tail $ shiftT 0 $ initialT : ms')

instance Pnset L where
  pnset nid ms = L $ \h g -> do
    let sn = Nset nid
    ms' <- unL (T.sequenceA $ M.fromList ms) h g
    return $ fmap (ToOSC sn) (tail $ shiftT 0 $ initialT : ms')

instance Pmerge L where
  pmerge = liftL2 (mergeL 0 (0,0))

instance Ppar L where
  ppar = foldr1 pmerge

instance Pdrop L where
  pdropT = predTL (<=)

instance Ptake L where
  ptakeT = predTL (>)
