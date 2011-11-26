{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6, 2011
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Performs fft of repa array data via FFTW.

Currently supporting DIM1 (Complex Double) arrays only.

-}

module Data.Array.Repa.FFTW
  ( -- * Examples
    -- $examples

    -- * FFT functions (1 dimension)
    fft
  , ifft

    -- * References
    -- $references

  ) where

import Data.Complex (Complex(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.CArray (CArray)
import Data.Array.Repa ((:.)(..), Array, Elt(..), DIM1, Z(..))
import Foreign.Storable.Complex ()

import qualified Data.Array.CArray as C
import qualified Data.Array.CArray.Base as C
import qualified Data.Array.Repa as R
import qualified Math.FFT as F

{-$examples

Sample module:

> import Data.Complex
> import Data.Array.Repa
> import Data.Array.Repa.FFTW
>
> a :: Array DIM1 (Complex Double)
> a = fromList (Z :. 5) [i :+ 0 | i <- [0..4]]

Loading above in ghci:

>>> a
Array (Z :. 5) [0.0 :+ 0.0,1.0 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0,4.0 :+ 0.0]
>>> fft a
Array (Z :. 5)
  [ 10.0 :+ 0.0
  , (-2.5) :+ 3.4409548011779334
  , (-2.5) :+ 0.8122992405822659
  , (-2.5) :+ (-0.8122992405822659)
  , (-2.5) :+ (-3.4409548011779334)
  ]
>>> ifft $ fft a
Array (Z :. 5) [0.0 :+ 0.0,1.0000000000000002 :+ 0.0,2.0 :+ 0.0,3.0 :+ 0.0,4.0 :+ 0.0]

-}

{-$references

* fftw : <http://www.fftw.org>

* fftw haskell binding : <http://hackage.haskell.org/package/fft>

-}

-- --------------------------------------------------------------------------
-- Exposed functions
--

-- | Performs 1 dimension forward fft.
fft :: Array DIM1 (Complex Double) -> Array DIM1 (Complex Double)
fft = c2r . F.dft . r2c
{-# INLINE fft #-}

-- | Performs 1 dimension inverse fft.
ifft :: Array DIM1 (Complex Double) -> Array DIM1 (Complex Double)
ifft = c2r . F.idft . r2c
{-# INLINE ifft #-}

-- --------------------------------------------------------------------------
-- Guts

r2c :: Array DIM1 (Complex Double) -> CArray Int (Complex Double)
r2c rarr = unsafePerformIO $ do
  let _:.nelem = R.extent rarr
  fptr <- C.mallocForeignPtrArrayAligned nelem
  withForeignPtr fptr $ \ptr ->
    R.withManifest' rarr $ \rarr' ->
      let go i
            | i == nelem = return ()
            | otherwise  = pokeElemOff ptr i (rarr' R.! (Z:.i)) >> go (i+1)
      in  go 0
  C.unsafeForeignPtrToCArray fptr (0,nelem-1)
{-# INLINE r2c #-}

c2r :: CArray Int (Complex Double) -> Array DIM1 (Complex Double)
c2r carr = case C.toForeignPtr carr of
  (n,fptr) -> let sh = Z:.n in
    R.fromFunction sh $ \ix ->
    unsafePerformIO $ withForeignPtr fptr $ \ptr ->
    peekElemOff ptr $ R.toIndex sh ix
{-# INLINE c2r #-}

instance (RealFloat a, Elt a) => Elt (Complex a) where
  {-# INLINE touch #-}
  touch (i :+ j) = touch i >> touch j
  {-# INLINE zero #-}
  zero = 0 :+ 0
  {-# INLINE one #-}
  one = 1 :+ 0
