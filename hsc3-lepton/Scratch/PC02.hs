{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Pattern classes with kind (* -> * -> *), with fixed type,
no class constraints.
-}
module Scratch.PC02 where

import Sound.SC3 (AddAction)

import Sound.SC3.Lepton.Pattern.ToOSC (ToOSC)
import Scratch.Type00

class Pint p where
  pint :: Int -> p h Int

  (+!)     :: p h Int -> p h Int -> p h Int
  (*!)     :: p h Int -> p h Int -> p h Int
  (-!)     :: p h Int -> p h Int -> p h Int
  pinegate :: p h Int -> p h Int
  piabs    :: p h Int -> p h Int
  pisignum :: p h Int -> p h Int

  pirange :: p h Int -> p h Int -> p h Int

class Pdouble p where
  pdouble :: Double -> p h Double
  (+@)     :: p h Double -> p h Double -> p h Double
  (*@)     :: p h Double -> p h Double -> p h Double
  (-@)     :: p h Double -> p h Double -> p h Double
  pdnegate :: p h Double -> p h Double
  pdabs    :: p h Double -> p h Double
  pdsignum :: p h Double -> p h Double

  pdrange :: p h Double -> p h Double -> p h Double

  (/@)   :: p h Double -> p h Double -> p h Double
  precip :: p h Double -> p h Double

  ppi   :: p h Double
  pexp  :: p h Double -> p h Double
  psqrt :: p h Double -> p h Double
  plog :: p h Double -> p h Double
  (**@) :: p h Double -> p h Double -> p h Double
  plogBase :: p h Double -> p h Double -> p h Double
  psin :: p h Double -> p h Double
  ptan :: p h Double -> p h Double
  pcos :: p h Double -> p h Double
  pasin :: p h Double -> p h Double
  patan :: p h Double -> p h Double
  pacos :: p h Double -> p h Double
  psinh :: p h Double -> p h Double
  ptanh :: p h Double -> p h Double
  pcosh :: p h Double -> p h Double
  pasinh :: p h Double -> p h Double
  patanh :: p h Double -> p h Double
  pacosh :: p h Double -> p h Double

  pampDb     :: p h Double -> p h Double
  pasFloat   :: p h Double -> p h Double
  pasInt     :: p h Double -> p h Double
  pbitNot    :: p h Double -> p h Double
  pcpsMIDI   :: p h Double -> p h Double
  pcpsOct    :: p h Double -> p h Double
  pcubed     :: p h Double -> p h Double
  pdbAmp     :: p h Double -> p h Double
  pdistort   :: p h Double -> p h Double
  pfrac      :: p h Double -> p h Double
  pisNil     :: p h Double -> p h Double
  plog10     :: p h Double -> p h Double
  plog2      :: p h Double -> p h Double
  pmidiCPS   :: p h Double -> p h Double
  pmidiRatio :: p h Double -> p h Double
  pnotE      :: p h Double -> p h Double
  pnotNil    :: p h Double -> p h Double
  poctCPS    :: p h Double -> p h Double
  pramp_     :: p h Double -> p h Double
  pratioMIDI :: p h Double -> p h Double
  psoftClip  :: p h Double -> p h Double
  psquared   :: p h Double -> p h Double

class Pappend p where
  pappend :: p h a -> p h a -> p h a

class Pconcat p where
  pconcat :: [p h a] -> p h a

class Preplicate p where
  preplicate :: p h Int -> p h a -> p h a

class Pseq p where
  pseq :: p h Int -> [p h a] -> p h a

class Pforever p where
  pforever :: p h a -> p h a

class Pcycle p where
  pcycle :: [p h a] -> p h a

class Prand p where
  prand :: p h Int -> [p h a] -> p h a

class Pshuffle p where
  pshuffle :: [p h a] -> p h a

class Ptuple p where
  pzip :: p h a -> p h b -> p h (a,b)
  pfst :: p h (a,b) -> p h a
  psnd :: p h (a,b) -> p h b

class Plambda p where
  pz   :: p (a,h) a
  ps   :: p h a -> p (any,h) a
  plam :: Ty a -> p (a,h) b -> p h (a->[b])
  -- plam :: TR a -> p (a,h) b -> p h (a->[b])
  papp :: p h (a->[b]) -> p h a -> p h b

class Psnew p where
  psnew :: String -> Maybe Int -> AddAction -> Int -> [(String, p r Double)]
           -> p r (ToOSC Double)

-- class Pnset p where ...

class Pmerge p where
  pmerge :: p h (ToOSC Double) -> p h (ToOSC Double) -> p h (ToOSC Double)

class Ppar p where
  ppar :: [p h (ToOSC Double)] -> p h (ToOSC Double)