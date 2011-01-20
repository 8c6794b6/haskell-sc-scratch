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
-- Concept was inspired from sclang, and Sound.SC3.Pattern in
-- hsc3-lang. Implementation was inspired from  Oleg Kiselyov's TTF
-- (<http://okmij.org/ftp/tagless-final/>).
--
-- There is no intention to translate entire set of Pattern
-- classes found in sclang. Lazy pure functional language like haskell has
-- different style for handling sequential data from OOP language like sclang.
--
module Sound.SC3.Lepton.Pattern
  ( -- * Examples

    -- ** Create, view, and run
    -- $example_intro

    -- ** Pattern used as musical event
    -- $example_spe

    -- ** Grouping and traversing
    -- $example_combine

    -- ** Low level tweaks
    -- $example_low_level

    -- * Expressions

    -- ** Fundamental
    Pval(..), Pempty(..), Plist(..),

    -- ** Looping
    Pseq(..), Pcycle(..),

    -- ** Random
    Prandom(..), Prange(..), Pchoose(..), Prand(..),

    -- ** Lambda and application
    Plam(..), Papp(..),

    -- * Interpreters

    -- ** Running patterns
    R(..), runP, runPIO,

    -- ** Showing patterns
    S(..), showP,

    -- -- Viewing patterns
    -- V(..), viewP,

  ) where

import Control.Applicative
import Data.Data -- (Typeable(..), Typeable1(..), mkAppTy, mkTyCon, mkTyConApp)
import Data.Map (Map)
import System.Random
import qualified Data.Map as M

-- $example_intro
--
-- Making pattern in ghci:
--
-- > > :set -XNoMonomorphismRestriction
-- > > let p1 = prand (pval 3) [pval 10, plist [1..5]]
--
-- Viewing the pattern:
--
-- > > viewP p1
-- > "prand (pval 3) [pval 10,plist [1,2,3,4,5]]"
--
-- And running it:
--
-- > > runPIO p1 -- try several times
-- > [10,1,2,3,4,5,10]
--
-- The type of this pattern is:
--
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
-- > -- pspe
-- > --   :: (Num (p Int),
-- > --       Pempty p,
-- > --       Num a,
-- > --       Plist p,
-- > --       Pchoose p,
-- > --       Prange p,
-- > --       Num (p a),
-- > --       Pseq p,
-- > --       Pcycle p) =>
-- > --      p a
-- > pspe =
-- >   pcycle
-- >     [pchoose 1
-- >        [pempty, plist [24,31,36,43,48,55]]
-- >     ,pseq (prange 2 5)
-- >        [60, pchoose 1 [63,65], 67, pchoose 1 [70,72,74]]
-- >     ,pchoose (prange 3 9)
-- >        [74,75,77,79,81]]
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

-- $example_combine
--
-- Group with Data.Map and traverse with @sequenceA@ from Data.Traverse.
--
-- > import Control.Concurrent (threadDelay)
-- > import Data.Map ((!))
-- > import Data.Traversable (sequenceA)
-- > import qualified Data.Map as M
-- >
-- > import Sound.SC3
-- > import Sound.SC3.Lepton
-- >
-- > main :: IO ()
-- > main = withSC3 goBuzz
-- >
-- > -- | Play the pattern.
-- > goBuzz :: (Transport t) => t -> IO ()
-- > goBuzz fd = do
-- >   async fd $ d_recv $ synthdef "buzz" buzz
-- >   ps <- runPIO . sequenceA . M.fromList $ pBuzz
-- >   mapM_ f ps
-- >   where
-- >     f m = do
-- >       send fd $ s_new "buzz" (-1) AddToTail 1 (M.assocs m)
-- >       threadDelay $ floor $ (m ! "dur") * 1e6 * (60/bpm)
-- >     bpm = 160
-- >
-- > -- | Ugen with having amp, freq, pan controls.
-- > buzz :: UGen
-- > buzz = out 0 $ pan2 sig pan 1
-- >   where
-- >     sig = sinOsc ar freq 0 * amp * e
-- >     e = linen tr 5e-3 1 (10e-3+(330/freq)) RemoveSynth ^ 3
-- >     amp = control kr "amp" 0.3
-- >     freq = control kr "freq" 440
-- >     pan = control kr "pan" 0
-- >     tr = tr_control "t_trig" 1
-- >
-- > -- Pattern for amp, dur, freq, and pan.
-- > pBuzz =
-- >   [("amp", pcycle [0.3, 0.1,  0.1,   0.3,  0.1,  0.1,  0.1])
-- >   ,("dur", pcycle [1,   0.55, 0.45,  0.54, 0.46, 0.53, 0.47])
-- >   ,("freq", fmap midiCPS $
-- >             pcycle [48, prand 13 cm, 53, prand 13 fm
-- >                    ,48, prand 13 cm, 43, prand 13 g7])
-- >   ,("pan", pcycle [plist [-1,-0.95..1], plist [1,0.95..(-1)]])]
-- >   where
-- >     cm = [55, 67,72,75,79]
-- >     fm = [60, 68,72,77,80]
-- >     g7 = [50, 67,71,74,77]
-- >

-- $example_low_level
--
-- Directly writing function for R:
--
-- > > runPIO $ pseq 3 [1,2,R $ \_ -> [999,1024]]
-- > [1,2,999,1024,1,2,999,1024,1,2,999,1024]
--
-- For S:
--
-- > > viewP $ prand (S $ \_ -> "foo bar buzz") [1..5]
-- > "prand (foo bar buzz) [pval 1,pval 2,pval 3,pval 4,pval 5]"
--
------------------------------------------------------------------------------
--
-- Expressions
--
------------------------------------------------------------------------------

-- | Lifts given value to pattern.
class Pval p where
  pval :: a -> p a

-- | Empty pattern.
class Pempty p where
  pempty :: p a

-- | Make sequence from given list.
class Plist p where
  plist :: [a] -> p a

-- | Sequential patterns.
class Pseq p where
  pseq :: p Int -> [p a] -> p a

-- | Random patterns.
class Prand p where
  prand :: p Int -> [p a] -> p a

class Prandom p where
  prandom :: (Random r) => p r

class Prange p where
  prange  :: (Random r) => p r -> p r -> p r

class Pchoose p where
  pchoose :: p Int -> [p r] -> p r

-- | Loop given list of patterns infinitely
class Pcycle p where
  pcycle :: [p a] -> p a

-- | Loop given value infinitely.
class Prepeat p where
  prepeat :: a -> p a

-- | Loop given pattern infinitely.
class Ploop p where
  ploop :: p a -> p a

class Plam p where
  plam :: (p a -> p b) -> p (a->b)

class Papp p where
  papp :: p (a->b) -> p a -> p b

-- | Generates infinite list of RandomGen.
gens :: (RandomGen g) => g -> [g]
gens = iterate (snd . next)

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

rlam :: (R a -> R b) -> R (a->b)
rlam f = R $ \g -> undefined
  -- unR $ f (

rlam' f = R (\g -> map (+) [1..10])

------------------------------------------------------------------------------
--
-- Imterpreters
--
------------------------------------------------------------------------------

-- | \"R\" for running patterns.
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

-- | Note that enumeration for floating point values are not working,
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

-- | Repeats the list of pattern with given number.
instance Pseq R where
  pseq n ps = R $ \g ->
    let ps' = concatMap (\n' -> concat $ replicate n' ps) (unR n g)
    in  concat $ zipWith unR ps' (gens g)

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

-- | \"S\" for showing patterns.
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

-- | Enumeration for floating points are now working here also.
-- fromEnum and toEnum are assuming pval only.
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

instance Pseq S where
  pseq n ps = S $ \_ -> "pseq (" ++ showP n ++ ") " ++ showList ps ""

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

-- instance Plam V where
vlam f = V $ "plam " ++ unV (f (V ""))
