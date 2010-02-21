{-# LANGUAGE GADTs #-}
------------------------------------------------------------------------------
-- | Trying to implement patterns with datatype.
--
-- XXX: What could be useful haskell representation for SClang's Pbind?

module Scratch.Pat.ByDatatype where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import System.Random

data Pat a = Pempty
           | Pid a
           | Pseq [Pat a] (Pat Int)
           | Prand [Pat a] (Pat Int)
           -- | Pbind [(String, Pat a)]
           | Pinf a
             deriving (Eq, Show, Read)

instance Functor Pat where
    fmap f Pempty = Pempty
    fmap f (Pid a) = Pid (f a)
    fmap f (Pseq ps p) = Pseq (fmap (fmap f) ps) p
    fmap f (Prand ps p) = Prand (fmap (fmap f) ps) p
    fmap f (Pinf a) = Pinf (f a)

instance Monoid (Pat a) where
    mempty = Pempty
    p1 `mappend` p2 = Pseq [p1,p2] (Pid 1)

instance (Num a) => Num (Pat a) where
    (+) = undefined
    (*) = undefined
    (-) = undefined
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = Pid . fromInteger

instance (Enum a) => Enum (Pat a) where
    succ = fmap succ
    fromEnum (Pid a) = fromEnum a 
    toEnum = Pid . toEnum

-- instance Enum (Pat Integer) where
--     succ = fmap (+1)

-- runP :: RandomGen g => g -> Pat a -> [a]
-- runP _ Pempty = []
-- runP _ (Pid x) = [x]
-- runP g (Pseq xs n) = 
--     concat $ concatMap (\y -> replicate y (concatMap (runP g) xs)) ys
--     where ys = runP g' n
--           (_,g') = next g
-- runP g (Prand xs p) = undefined

runP :: RandomGen g => Pat a -> g -> ([a],g)
runP pat g = runState (mkPat pat) g 

evalP :: RandomGen g => g -> Pat a -> [a]
evalP g pat = evalState (mkPat pat) g

mkPat :: RandomGen g => Pat a -> State g [a]
mkpat Pempty = return []
mkPat (Pid a) = return [a]
mkPat (Pseq ps p) = do
  g <- get
  let v = []
      f a b = do
        g <- get
        let (_,g') = next g
        put g'
        return $ a ++ evalP g b
      ys = evalP g p
  fmap (join . join) $ mapM (\z -> replicateM z $ foldM f v ps) ys
mkPat (Prand ps p) = do
  g <- get 
  let ys = evalP g p
      (_,g') = next g
  put g'
  fmap (join . join) $ mapM (\y -> replicateM y $ f ps) ys
    where
      f ps = do
        g <- get
        let (a,g') = chooseOne ps g
        put g'
        return $ evalP g a
mkPat (Pinf a) = return (repeat a)

chooseOne :: RandomGen g => [a] -> g -> (a,g)
chooseOne xs g = (xs !! idx, g')
    where (idx,g') = randomR (0,length xs - 1) g

pat01 = Pseq [1..5] 2

pat02 = 
  Prand 
    [Pseq [1..3] 1,
     Pseq [10,20,30] 1,
     Pseq [100,200,300] 1] 1

pat03 = 
  Pseq
    [Prand [1,2,3,4,5] 5,
     Prand [10,20,30] 3,
     Prand [100,200] 2]
    2

pat04 =
  Pseq 
    [Pseq [1,2,3,4,5] 3,
     Prand [1..5] 5] 
    2
    