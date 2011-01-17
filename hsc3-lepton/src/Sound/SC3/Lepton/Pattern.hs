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
-- Pattern DSL.
--
-- Concept was inspired from sclang, implementation was inspired from
-- Oleg Kiselyov's TTF (<http://okmij.org/ftp/tagless-final/>).
--
-- There is no intention to translate entire set of Pattern
-- classes found in sclang. Lazy pure functional language like haskell has
-- different style for handling sequential data, compared to OOP language like sclang.
--
-- Rank2Types and FlexibleContexts language pragmes are used for making instance
-- of newtype S with types deriving Show class.
--
module Sound.SC3.Lepton.Pattern
  ( -- * Examples

    -- ** Create, view, and run
    -- $example_intro

    -- ** Pattern used as musical event
    -- $example_spe

    -- ** Low level tweaks
    -- $example_low_level

    -- * Interpreters
    R(..), runP, runPIO,
    S(..), viewP,

    -- * Expressions
    Pval(..), Pempty(..), Plist(..),
    Pseq(..), Prand(..), Pcycle(..)
  ) where

import Control.Applicative
import Control.Monad (ap)
import Data.Char (isDigit)
import Data.List (intersperse)
import System.Random

-- $example_intro
--
-- In ghci:
--
-- > > :set -XNoMonomorphismRestriction
-- > > let p1 = prand (pval 3) [pval 10, plist [1..5]]
-- > > viewP p1
-- > "prand (pval 3) [pval 10, plist [1,2,3,4,5]]"
-- > > runPIO p1 -- try several times
-- > [10,1,2,3,4,5,10]
-- > > :t p1
-- > p1 :: (Pval p, Num t, Enum t, Plist p, Prand p) => p t
--

-- $example_spe
--
-- Translation of \"/Understanding Streams, Patterns, and Events - Part 3/\"
-- example from supercollider help file.
--
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import Control.Concurrent (threadDelay)
-- > import System.Random (newStdGen, randomRs)
-- >
-- > import Sound.OpenSoundControl
-- > import Sound.SC3
-- > import Sound.SC3.ID
-- >
-- > import Sound.SC3.Lepton.Pattern
-- >
-- > main :: IO ()
-- > main = withSC3 go
-- >
-- > -- | Load synthdef and play the pattern.
-- > go :: (Transport t) => t -> IO ()
-- > go fd = do
-- >   async fd . d_recv . synthdef "speSynth" =<< speSynth
-- >   mapM_ f =<< runPIO pspe
-- >   where
-- >     f v = do
-- >       send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
-- >       threadDelay (floor $ 0.13 * 1e6)
-- >
-- > -- | Synthdef for spe example.
-- > speSynth :: IO UGen
-- > speSynth = do
-- >   dl <- randomRs (0,0.05) `fmap` newStdGen
-- >   dr <- randomRs (0,0.05) `fmap` newStdGen
-- >   return $ out 0 $ mkSig dl dr
-- >   where
-- >     mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
-- >     v = rlpf (lfSaw ar freq 0 * evl) nz 0.1
-- >     f a b = allpassN b 0.05 a 4
-- >     evl = envGen kr 1 1 0 1 RemoveSynth shp * 0.3
-- >     shp = envPerc 10e-3 1
-- >     nz = midiCPS (lfNoise1 'z' kr 1 * 36 + 110)
-- >     freq = control kr "freq" 440
-- >
-- > -- Pattern used for pitches of sound.
-- > pspe =
-- >   pcycle
-- >     [prand 1 [pempty, plist [24,31,36,43,48,55]]
-- >     ,pseq (prand 1 [2..5])
-- >        [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
-- >     ,prand (prand 1 [3..9]) [74,75,77,79,81]]
--
-- Below is the original pattern written in sclang:
--
-- > freqStream = Pseq([
-- >    Prand([
-- >       nil, // a nil item reached in a pattern causes it to end
-- >       Pseq(#[24, 31, 36, 43, 48, 55]);
-- >    ]),
-- >    Pseq([ 60, Prand(#[63, 65]), 67, Prand(#[70, 72, 74]) ], { rrand(2, 5) }),
-- >    Prand(#[74, 75, 77, 79, 81], { rrand(3, 9) })
-- > ], inf).asStream.midicps;
--

-- $example_low_level
--
-- Writing funcsions for R and S directlry:
--
-- > > runPIO $ prand 3 [1,2,R $ \_ -> [999,1024]]
-- > [1,1,999,1024]
-- > > viewP $ prand (S $ \_ -> "blah blah blah") [1..5]
-- > "prand (blah blah blah) [pval 1,pval 2,pval 3,pval 4,pval 5]"
--

------------------------------------------------------------------------------
--
-- Imterpreters
--
------------------------------------------------------------------------------

-- | \"R\" for running patterns.
newtype R a = R {unR :: StdGen -> [a]}

instance (Eq a) => Eq (R a) where
  -- undefined !!
  a == b = undefined

instance (Show a) => Show (R a) where
  show _ = "R"

instance Functor R where
  fmap f (R r) = R $ \g -> fmap f (r g)

instance Monad R where
  return a = R $ \_ -> [a]
  (R r) >>= k = R $ \g -> concatMap (\x -> unR (k x) g) (r g)

-- | Behaves like ZipList.
instance Applicative R where
  pure x = R $ \_ -> repeat x
  R rf <*> R rv = R $ \g -> zipWith id (rf g) (rv g)

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

-- | Run pattern with given seed.
runP :: R a -> StdGen -> [a]
runP p g = unR p g

-- | Run pattern with new seed.
runPIO :: R a -> IO [a]
runPIO p = unR p `fmap` newStdGen


-- | \"S\" for showing patterns.
newtype S a = S {unS :: Show a => forall a. a -> String}

instance (Show a, Eq a) => Eq (S a) where
  a == b = viewP a == viewP b

instance (Show a) => Show (S a) where
  show _ = "S"

instance (Num a) => Num (S a) where
  a + b = S $ \_ -> viewP a ++ " + " ++ viewP b
  a * b = S $ \_ -> viewP a ++ " * " ++ viewP b
  abs n = S $ \_ -> "abs (" ++ viewP n ++ ")"
  negate n = S $ \_ -> "negate (" ++ viewP n ++ ")"
  signum n = S $ \_ -> "signum (" ++ viewP n ++ ")"
  -- Plain numbers would be shown as pval.
  fromInteger n = S $ \_ -> "pval " ++ show (fromInteger n)

instance (Fractional a) => Fractional (S a) where
  a / b = S $ \_ -> viewP a ++ " / " ++ viewP b
  fromRational n = S $ \_ -> "fromRational " ++ show n

instance (Show a, Enum a) => Enum (S a) where
  pred n = S $ \_ -> "pred (" ++ viewP n ++ ")"
  succ n = S $ \_ -> "succ (" ++ viewP n ++ ")"
  -- fromEnum assuming "pval" only.
  fromEnum n = read $ filter isDigit (viewP n)
  -- toEnum assuming "pval" only.
  toEnum n = S $ \_ -> "pval " ++ show n

-- | Show string representation of pattern.
viewP :: (Show a) => S a -> String
viewP p = unS p ()


------------------------------------------------------------------------------
--
-- Expressions
--
------------------------------------------------------------------------------

-- | Lifts given value as pattern.
--
class Pval p where
  pval :: a -> p a

-- | Singleton list.
instance Pval R where
  pval a = R $ \_ -> [a]

instance Pval S where
  pval a = S $ \_ -> "pval " ++ show a

-- | Empty pattern.
class Pempty p where
  pempty :: p a

-- | Returns @[]@.
instance Pempty R where
  pempty = R $ \_ -> []

instance Pempty S where
  pempty = S $ \_ -> "pempty"

-- | Make sequence from given list.
class Plist p where
  plist :: [a] -> p a

instance Plist R where
  plist a = R $ \_ -> a

instance Plist S where
  plist a = S $ \_ -> "plist " ++ show a

-- | Sequential patterns.
class Pseq p where
  -- | Repeats the list of pattern with given number.
  pseq :: p Int -> [p a] -> p a

instance Pseq R where
  pseq n ps = R $ \g ->
    let f pat gen = unR pat gen
        gs = iterate (snd . next) g
        ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
    in concat $ zipWith f ps' gs

instance Pseq S where
  pseq n ps = S $ \x ->
    "pseq (" ++ unS n x ++ ") [" ++
    concat (intersperse "," $ map (\p -> unS p x) ps) ++ "]"

-- | Random patterns.
class Prand p where
  -- | Choose element from given list for number of given times.
  prand :: p Int -> [p a] -> p a

instance Prand R where
  prand n ps = R $ \g ->
    let gs = take (sum $ unR n g) $ iterate (snd . next) g
    in  concatMap (\h -> let (j,_) = randomR (0,length ps - 1) h
                         in  unR (ps!!j) h) gs

instance Prand S where
  prand n ps = S $ \x ->
    "prand (" ++ unS n x ++ ") [" ++
    concat (intersperse "," $ map (\p -> unS p x) ps) ++ "]"

-- | Looping patterns.
class Pcycle p where
  -- | Loop given list of patterns infinitely.
  pcycle :: [p a] -> p a

instance Pcycle R where
  pcycle ps = R $ \g ->
    let gs = iterate (snd . next) g
        f p h = unR p h
    in  concat $ zipWith f (cycle ps) gs

instance Pcycle S where
  pcycle ps = S $ \x ->
    "pcycle [" ++ concat (intersperse "," $ map (\p -> unS p x) ps) ++ "]"
