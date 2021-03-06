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
module Sound.SC3.Supply
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
  , supply

  -- * Lifting functions
  , liftS2
  , liftS3
  , liftS4
  , liftSquash
  , squash

  -- * UGen wrappers
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
  , sshuf
  , sstutter
  , sval
  , sswitch
  , sswitch1
  , swhite
  , swrand
  , sxrand
  ) where

import Control.Applicative hiding ((<*))
import Control.Monad.State
    (MonadState(..), State, evalState, get, put, runState)
import System.Random (StdGen, getStdRandom, mkStdGen, random)

import Sound.SC3.ID

{-$example

From /Streams-Patterns-Events/ tutorial in SuperCollider help files.

> import System.Random
>
> import Sound.SC3
> import Sound.SC3.ID
> import Sound.SC3.Supply
>
> main :: IO ()
> main = audition $ playS sup
>
> playS :: Supply -> UGen
> playS sp = out 0 sig
>   where
>     sig   = foldr f v (zipWith mce2 (mkRs "abcd") (mkRs "efgh"))
>     f a b = allpassN b 0.05 a 4
>     v     = rlpf (pulse AR (mce2 freq (freq*1.01)) bw) rq cf * amp
>     rq    = lfdNoise3 'n' KR 2.323 * 2000 + 2200
>     cf    = lfdNoise3 'q' KR 1.110 * 0.498 + 0.5
>     bw    = lfdNoise3 'b' KR 0.1123 * 0.48 + 0.5
>     mkRs  = map (\x -> rand x 0.001 0.1)
>     freq  = midiCPS $ demand tick 0 (evalSupply sp (mkStdGen 0x81aafad))
>     amp   = decay2 tick 5e-4 950e-3 * 0.2
>     tick  = impulse KR 7.6923 0
>
> sup :: Supply
> sup =
>   sseq sinf
>   [srand 1
>    [snil, sseq 1 [24,31,36,43,48,55]]
>   ,sseq (siwhite sinf 2 5)
>    [60, srand 1 [63,65], 67, srand 1 [70,72,74]]
>   ,srand (siwhite sinf 3 9)
>    [74,75,77,79,81]]

-}

-- | Wrapper for demand ugens.
newtype Demand a = Demand {unDemand :: State StdGen a}
     deriving (Functor, Applicative, Monad, MonadState StdGen)

-- | Fundamental type for constructing pattern with demand ugen.
type Supply = Demand UGen

-- | Unwrap 'Demand' with `mkStdGen 0`.
evalDemand0 :: Demand a -> a
evalDemand0 d = evalState (unDemand d) (mkStdGen 0)

-- | Compare with running state using (mkStdGen 0).
instance Eq a => Eq (Demand a) where
  Demand a == Demand b = let g = mkStdGen 0 in evalState a g == evalState b g

-- | Show with evaluating state using (mkStdGen 0).
instance Show a => Show (Demand a) where
  show a = "Demand (" ++ (show $ evalDemand0 a) ++ ")"

instance Fractional a => Fractional (Demand a) where
  a / b        = (/) <$> a <*> b
  recip a      = fmap recip a
  fromRational = return . fromRational

instance Floating a => Floating (Demand a) where
    pi     = return pi
    exp    = fmap exp
    sqrt   = fmap sqrt
    log    = fmap log
    a ** b = (**) <$> a <*> b
    sin    = fmap sin
    cos    = fmap cos
    asin   = fmap asin
    atan   = fmap atan
    acos   = fmap acos
    sinh   = fmap sinh
    tanh   = fmap tanh
    cosh   = fmap cosh
    asinh  = fmap asinh
    atanh  = fmap atanh
    acosh  = fmap acosh

instance Enum a => Enum (Demand a) where
    succ = fmap succ
    pred = fmap pred
    fromEnum = fromEnum . evalDemand0
    toEnum = return . toEnum
    enumFrom = fmap return . enumFrom . evalDemand0
    enumFromThen a b = fmap return . evalDemand0 $ liftA2 enumFromThen a b
    enumFromTo a b = fmap return . evalDemand0 $ liftA2 enumFromTo a b
    enumFromThenTo a b c =
        fmap return . evalDemand0 $ liftA3 enumFromThenTo a b c

instance Integral a => Integral (Demand a) where
    quot = liftA2 quot
    rem = liftA2 rem
    div = liftA2 div
    mod = liftA2 mod
    quotRem a b = (quot a b, rem a b)
    toInteger d = toInteger $ evalDemand0 d

instance Num a => Num (Demand a) where
    a + b = (+) <$> a <*> b
    a - b = (-) <$> a <*> b
    a * b = (*) <$> a <*> b
    negate a = fmap negate a
    abs a = fmap abs a
    signum a = fmap signum a
    fromInteger = return . fromInteger

instance Real a => Real (Demand a) where
    toRational = toRational . evalDemand0

instance RealFrac a => RealFrac (Demand a) where
    properFraction = error "Demand.properFraction"
    round = round . evalDemand0
    ceiling = ceiling . evalDemand0
    floor = floor . evalDemand0

instance Ord a => Ord (Demand a) where
    compare a b = evalDemand0 (liftA2 compare a b)

instance OrdE a => OrdE (Demand a) where
    a <*  b = (<*)  <$> a <*> b
    a <=* b = (<=*) <$> a <*> b
    a >*  b = (>*)  <$> a <*> b
    a >=* b = (>=*) <$> a <*> b

instance UnaryOp a => UnaryOp (Demand a) where
    ampDb = fmap ampDb
    asFloat = fmap asFloat
    asInt = fmap asInt
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

instance RealFracE a => RealFracE (Demand a) where

instance EqE a => EqE (Demand a) where
    (==*) = liftA2 (==*)
    (/=*) = liftA2 (/=*)

-- | Run demand ugen with given StdGen.
runSupply :: Supply -> StdGen -> (UGen,StdGen)
runSupply d g = runState (unDemand d) g

-- | Run demand ugen with new StdGen.
runSupplyIO :: Supply -> IO UGen
runSupplyIO = getStdRandom . runSupply

-- | Evaluate demand ugen with given StdGen.
evalSupply :: Supply -> StdGen -> UGen
evalSupply d g = evalState (unDemand d) g

-- | Evaluate demand ugen with given seed.
supply :: Int -> Supply -> UGen
supply i s = evalState (unDemand s) (mkStdGen i)

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

-- | Wrapper for 'dbrown'.
sbrown :: Supply -> Supply -> Supply -> Supply -> Supply
sbrown = liftS4 dbrown

-- | Wrapper for 'dbufrd'.
sbufrd :: Supply -> Supply -> Loop -> Supply
sbufrd buf phs loop = dbufrd <$> seed <*> buf <*> phs <*> return loop

-- | Wrapper for 'dbufwr'
sbufwr :: Supply -> Supply -> Supply -> Loop -> Supply
sbufwr buf phs input loop =
  dbufwr <$> seed <*> buf <*> phs <*> input <*> return loop

-- | Wrapper for 'dgeom'.
sgeom :: Supply -> Supply -> Supply -> Supply
sgeom = liftS3 dgeom

-- | Wrapper for 'dibrown'.
sibrown :: Supply -> Supply -> Supply -> Supply -> Supply
sibrown = liftS4 dibrown

-- | Wrapper for 'dinf'.
sinf :: Supply
sinf = return dinf

-- | Wrapper for 'diwhite'.
siwhite :: Supply -> Supply -> Supply -> Supply
siwhite = liftS3 diwhite

-- | Counterpart of zero length demand sequence.
snil :: Supply
snil = sseq 0 [0]

-- | Wrapper for 'drand'.
srand :: Supply -> [Supply] -> Supply
srand = liftSquash drand

-- | Wrapper for 'dseq'.
sseq :: Supply -> [Supply] -> Supply
sseq = liftSquash dseq

-- | Wrapper for 'dser'.
sser :: Supply -> [Supply] -> Supply
sser = liftSquash dser

-- | Wrapper for 'dseries'.
sseries :: Supply -> Supply -> Supply -> Supply
sseries = liftS3 dseries

-- | Wrapper for 'dshuf'.
sshuf :: Supply -> [Supply] -> Supply
sshuf = liftSquash dshuf

-- | Wrapper for 'dstutter'.
sstutter :: Supply -> Supply -> Supply
sstutter = liftS2 dstutter

-- | Lift given ugen as Demand UGen.
sval :: UGen -> Supply
sval = return

-- | Wrapper for 'dswitch'.
sswitch :: Supply -> [Supply] -> Supply
sswitch = liftSquash dswitch

-- | Wrapper for 'dswitch1'.
sswitch1 :: Supply -> [Supply] -> Supply
sswitch1 = liftSquash dswitch1

-- | Wrapper for 'dwhite'.
swhite :: Supply -> Supply -> Supply -> Supply
swhite = liftS3 dwhite

-- | Wrapper for 'dwrand'.
swrand :: Supply -> [Supply] -> [Supply] -> Supply
swrand dur vals probs = liftS3 dwrand dur (squash vals) (squash probs)

-- | Wrapper for 'dxrand'.
sxrand :: Supply -> [Supply] -> Supply
sxrand = liftSquash dxrand
