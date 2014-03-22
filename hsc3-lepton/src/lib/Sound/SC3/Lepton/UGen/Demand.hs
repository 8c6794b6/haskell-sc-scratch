{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GeneralizedNewtypeDeriving)

State monad wrapper for building patterns with demand ugens.

-}
module Sound.SC3.Lepton.UGen.Demand
  (
  -- * Example
  -- $example

  -- * Types
    Demand
  , Supply

  -- * Unwrappers
  , runSupply
  , runSupplyIO
  , evalSupply

  -- * Lifting functions
  , liftS2
  , liftS3
  , liftS4
  , liftSquash
  , squash

  -- * Building blocks
  , seed
  , sbrown
  , sbufrd
  , sbufwr
  , sgeom
  , sibrown
  , sinf
  , siwhite
  , snil
  , srand
  , sseq
  , sser
  , sseries
  , sstutter
  , sval
  , sswitch
  , sswitch1
  , swhite
  , sxrand
  ) where

import Control.Applicative
import Control.Monad.State
import System.Random

import Sound.SC3
import Sound.SC3.ID

-- $example
--
-- From /Streams-Patterns-Events/ tutorial in SuperCollider help files.
--
-- > import System.Random
-- >
-- > import Sound.SC3
-- > import Sound.SC3.ID
-- > import Sound.SC3.Lepton.UGen
-- >
-- > main :: IO ()
-- > main = audition $ playS sup
-- >
-- > playS :: Supply -> UGen
-- > playS sp = out 0 $ foldr f v (zipWith mce2 rs1 rs2) where
-- >   v = rlpf (pulse ar (mce2 freq (freq*1.01)) bw * 0.2 * amp)
-- >       (lfdNoise3 'n' kr 2.323 * 2000 + 2200)
-- >       (lfdNoise3 'q' kr 1.110 * 0.498 + 0.5)
-- >   f a b = allpassN b 0.05 a 4
-- >   rs1 = map mkR "abcd"
-- >   rs2 = map mkR "efgh"
-- >   mkR x = rand x 0.001 0.05
-- >   freq = midiCPS $ demand tick 0 (evalSupply sp (mkStdGen 0x81aafad))
-- >   amp = decay2 tick 5e-4 950e-3
-- >   tick = impulse kr 7.6923 0
-- >   bw = lfdNoise3 'b' kr 0.1123 * 0.48 + 0.5
-- >
-- > sup :: Supply
-- > sup =
-- >   sseq sinf
-- >   [srand 1
-- >    [snil, sseq 1 [24,31,36,43,48,55]]
-- >   ,sseq (siwhite sinf 2 5)
-- >    [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
-- >   ,srand (siwhite sinf 3 9)
-- >    [74,75,77,79,81]]
--
-- See also "Sound.SC3.Lepton.Pattern".
--

-- | Wrapper for demand ugens.
newtype Demand a = Demand {unDemand :: State StdGen a}
                 deriving (Functor, Applicative, Monad, MonadState StdGen)

-- | Fundamental type for constructing pattern with demand ugen.
type Supply = Demand UGen

-- | Compare with running state using (mkStdGen 0).
instance Eq a => Eq (Demand a) where
  Demand a == Demand b = let g = mkStdGen 0 in evalState a g == evalState b g

-- | Show with evaluating state using (mkStdGen 0).
instance Show a => Show (Demand a) where
  show (Demand a) = "Demand (" ++ (show $ evalState a (mkStdGen 0)) ++ ")"

instance Num a => Num (Demand a) where
  a + b = (+) <$> a <*> b
  a - b = (-) <$> a <*> b
  a * b = (*) <$> a <*> b
  negate a = fmap negate a
  abs a = fmap abs a
  signum a = fmap signum a
  fromInteger = return . fromInteger

instance Fractional a => Fractional (Demand a) where
  a / b = (/) <$> a <*> b
  recip a = fmap recip a
  fromRational = return . fromRational

instance Enum a => Enum (Demand a) where
  fromEnum d = fromEnum $ evalState (unDemand d) (mkStdGen 0)
  toEnum = return . toEnum

-- | Run demand ugen with given StdGen.
runSupply :: Supply -> StdGen -> (UGen,StdGen)
runSupply d g = runState (unDemand d) g

-- | Run demand ugen with new StdGen.
runSupplyIO :: Supply -> IO UGen
runSupplyIO = getStdRandom . runSupply

-- | Evaluate demand ugen with given StdGen.
evalSupply :: Supply -> StdGen -> UGen
evalSupply d g = evalState (unDemand d) g

liftS2 :: (Int -> a -> b -> c)
       -> Demand a -> Demand b -> Demand c
liftS2 f a b = f <$> seed <*> a <*> b

liftS3 :: (Int -> a -> b -> c -> d)
       -> Demand a -> Demand b -> Demand c -> Demand d
liftS3 f a b c = f <$> seed <*> a <*> b <*> c

liftS4 :: (Int -> a -> b -> c -> d -> e)
       -> Demand a -> Demand b -> Demand c -> Demand d -> Demand e
liftS4 f a b c d = f <$> seed <*> a <*> b <*> c <*> d

liftSquash :: (Int -> a -> UGen -> b) -> Demand a -> [Supply] -> Demand b
liftSquash f a b = f <$> seed <*> a <*> squash b

-- | Returns random Int value.
-- Passed as ID instance argument used in demand ugens.
seed :: Demand Int
seed = get >>= \g -> let (i,g') = random g in put g' >> return i

-- | Make array from given list of Demand UGens, with applying @mce@.
squash :: [Supply] -> Supply
squash = fmap mce . sequence

-- | Counterpart of dbrown
sbrown :: Supply -> Supply -> Supply -> Supply -> Supply
sbrown = liftS4 dbrown

-- | Counterpart of dbufrd
sbufrd :: Supply -> Supply -> Loop -> Supply
sbufrd buf phs loop = dbufrd <$> seed <*> buf <*> phs <*> return loop

-- | Counterpart of dbufwr
sbufwr :: Supply -> Supply -> Supply -> Loop -> Supply
sbufwr buf phs input loop =
  dbufwr <$> seed <*> buf <*> phs <*> input <*> return loop

-- | Counterpart of dgeom.
sgeom :: Supply -> Supply -> Supply -> Supply
sgeom = liftS3 dgeom

-- | Counterpart of dibrown.
sibrown :: Supply -> Supply -> Supply -> Supply -> Supply
sibrown = liftS4 dibrown

-- | Counterpart of dinf.
sinf :: Supply
sinf = return dinf

-- | Counterpart of diwhite.
siwhite :: Supply -> Supply -> Supply -> Supply
siwhite = liftS3 diwhite

-- | Counterpart of zero length demand sequence.
snil :: Supply
snil = sseq 0 [0]

-- | Counterpart of drand.
srand :: Supply -> [Supply] -> Supply
srand = liftSquash drand

-- | Counterpart of dseq.
sseq :: Supply -> [Supply] -> Supply
sseq = liftSquash dseq

-- | Counterpart of dser.
sser :: Supply -> [Supply] -> Supply
sser = liftSquash dser

-- | Counterpart of dseries.
sseries :: Supply -> Supply -> Supply -> Supply
sseries = liftS3 dseries

-- | Counterpart of dstutter.
sstutter :: Supply -> Supply -> Supply
sstutter = liftS2 dstutter

-- | Lift given ugen as Demand UGen.
sval :: UGen -> Supply
sval = return

-- | Counterpart of dswitch.
sswitch :: Supply -> [Supply] -> Supply
sswitch = liftSquash dswitch

-- | Counterpart of dswitch1.
sswitch1 :: Supply -> [Supply] -> Supply
sswitch1 = liftSquash dswitch1

-- | Counterpart of dwhite.
swhite :: Supply -> Supply -> Supply -> Supply
swhite = liftS3 dwhite

-- | Counterpart of dxrand.
sxrand :: Supply -> [Supply] -> Supply
sxrand = liftSquash dxrand
