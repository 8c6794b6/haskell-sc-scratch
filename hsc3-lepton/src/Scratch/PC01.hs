{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Pattern classes with kind (* -> * -> *), to take extra argument for VarEnv.
-}
module Scratch.PC01 where

import System.Random (Random)

import Data.Binary (Binary)
import Sound.SC3 (AddAction,UnaryOp)

import Sound.SC3.Lepton.Pattern.ToOSC (ToOSC)

class Pval p where
  pval :: (Binary a, Show a) => a -> p h a

class Plist p where
  plist :: (Binary a, Show a) => [a] -> p h a

class Prepeat p where
  prepeat :: (Binary a, Show a) => a -> p h a

class Pempty p where
  pempty :: p h a

class Pprim p where
  pdouble :: Double -> p h Double
  pint    :: Int -> p h Int
  pbool   :: Bool -> p h Bool

class Pappend p where
  pappend :: p h a -> p h a -> p h a

class Pconcat p where
  pconcat :: [p h a] -> p h a

class Prand p where
  prand :: p h Int -> [p h a] -> p h a

class Prange p where
  prange :: Random a => p h a -> p h a -> p h a

class Prng p where
  pirange :: p h Int -> p h Int -> p h Int
  pdrange :: p h Double -> p h Double -> p h Double

class Preplicate p where
  preplicate :: p h Int -> p h a -> p h a

class Pseq p where
  pseq :: p h Int -> [p h a] -> p h a

class Pforever p where
  pforever :: p h a -> p h a

class Pcycle p where
  pcycle :: [p h a] -> p h a

class Plc p where
  pz  :: p (a,h) a
  ps  :: p h a -> p (any,h) a
  lam :: p (a,h) b -> p h (a->[b])
  app :: p h (a->[b]) -> p h a -> p h b

class Psnew p where
  psnew :: String -> Maybe Int -> AddAction -> Int -> [(String, p r Double)]
           -> p r (ToOSC Double)

class Pnum p where
  padd :: Num a => p h a -> p h a -> p h a
  pmul :: Num a => p h a -> p h a -> p h a
  psub :: Num a => p h a -> p h a -> p h a
  pnegate :: Num a => p h a -> p h a
  pabs :: Num a => p h a -> p h a
  psignum :: Num a => p h a -> p h a
  pfromInteger :: Num a => Integer -> p h a

class Pfractional p where
  pdiv :: Fractional a => p h a -> p h a -> p h a
  precip :: Fractional a => p h a -> p h a
  pfromRational :: Fractional a => Rational -> p h a

class Pfloating p where
  pexp :: Floating a => p h a -> p h a
  psqrt :: Floating a => p h a -> p h a
  plog :: Floating a => p h a -> p h a

class Punary p where
  pmidiCPS :: UnaryOp a => p h a -> p h a

class Ptuple p where
  pzip :: p h a -> p h b -> p h (a,b)
  pfst :: p h (a,b) -> p h a
  psnd :: p h (a,b) -> p h b
