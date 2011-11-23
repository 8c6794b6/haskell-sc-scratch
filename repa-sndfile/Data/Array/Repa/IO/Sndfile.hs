{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Read and write audio file with repa arrays using libsndfile via hsndfile.

-}
module Data.Array.Repa.IO.Sndfile
  (
    -- * Example
    -- $example

    -- * Sound file reader and writer
    readSF
  , writeSF
  , withSF

    -- * Util
  , toMC
  , fromMC

    -- * Re-exports from hsndfile
  , S.Info(..)
  , S.Format(..)
  , S.HeaderFormat(..)
  , S.EndianFormat(..)
  , S.SampleFormat(..)
  , S.Count

  ) where

import Data.Word (Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)

import Data.Array.Repa (Array, DIM1, DIM2, Elt, Shape(..), Z(..), (:.)(..))
import Sound.File.Sndfile (Buffer(..), Info(..), Sample)

import qualified Data.Array.Repa as R
import qualified Sound.File.Sndfile as S

{-$example

Writing 440hz sine wav for 3 seconds to \"out.wav\".

> module Main where
>
> import Data.Array.Repa ((:.)(..), Array, DIM2, Z(..), fromFunction)
> import Data.Array.Repa.IO.Sndfile
>
> main :: IO ()
> main =
>   let dur = 3; freq = 440; sr = 48000
>
>       info = Info
>         { samplerate = sr
>         , frames = sr * dur
>         , channels = 1
>         , format = Format HeaderFormatWav SampleFormatPcm16 EndianFile
>         , sections = 1
>         , seekable = True }
>
>       sig :: Array DIM2 Double
>       sig = fromFunction (Z :. 1 :. dur * sr) $ \(_ :. _ :. i) ->
>         sin (fromIntegral i * freq * pi * 2 / fromIntegral sr)
>
>   in  writeSF "out.wav" info sig

Read \"out.wav\", write to \"copied.wav\" with same format.

> copy :: IO ()
> copy = do
>   (i, a) <- readSF "out.wav" :: IO (Info, Array DIM2 Double)
>   writeSF "copied.wav" i a

-}

-- ---------------------------------------------------------------------------
-- Wrapper actions

-- | Read sound file from given path.
--
-- Returns a tuple of Info and array containing the samples of sound
-- file.  Returned pair contains sound file information and array which
-- is indexed with channel number and frame.  Info could used later for
-- writing sound file.
--
readSF :: forall a. (Elt a, Sample a) => FilePath -> IO (Info, Array DIM2 a)
readSF path = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array DIM1 a))
  case arr of
    Nothing   -> error $ "readSF: failed reading " ++ path
    Just arr' -> return (info, toMC (S.channels info) arr')

{-# INLINE readSF #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array DIM2 Double) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array DIM2 Float) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array DIM2 Word16) #-}
{-# SPECIALIZE readSF :: FilePath -> IO (Info, Array DIM2 Word32) #-}

-- | Write array contents to sound file with given header information.
--
-- Expecting an array indexed with channel and frame, as returned from readSF.
-- i.e. 2-dimensional array with its contents indexed with channel.
--
writeSF ::
  forall a. (Elt a, Sample a) => FilePath -> Info -> Array DIM2 a -> IO ()
writeSF path info arr = do
  S.writeFile info path (fromMC arr)
  return ()

{-# INLINE writeSF #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array DIM2 Double -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array DIM2 Float -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array DIM2 Word16 -> IO () #-}
{-# SPECIALIZE writeSF :: FilePath -> Info -> Array DIM2 Word32-> IO () #-}

-- | Wrapper for invoking array with reading sound file.
--
-- Performs given action using sound file info and samples as arguments.
--
withSF ::
  forall a b. (Sample a, Elt a) =>
  FilePath -> (Info -> Array DIM2 a -> IO b) -> IO b
withSF path act = do
  (info, arr) <- S.readFile path :: IO (Info, Maybe (Array DIM1 a))
  case arr of
    Nothing   -> error ("withSF: failed to read " ++ path)
    Just arr' -> act info (toMC (S.channels info) arr')

{-# INLINE withSF #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array DIM2 Double -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array DIM2 Float -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array DIM2 Word16 -> IO b) -> IO b #-}
{-# SPECIALIZE withSF
  :: FilePath -> (Info -> Array DIM2 Word32 -> IO b) -> IO b #-}

-- ---------------------------------------------------------------------------
-- Internal work

-- | Orphan instance for reading/wriging sound file to array via ForeignPtr.
--
instance (Sample e, Elt e) => Buffer (Array DIM1) e where

  -- Read the whole contents to DIM1 array, ignoring channel number.
  --
  fromForeignPtr fptr _ count = return $ unsafeFFP (Z :. count) fptr

  {-# INLINE fromForeignPtr #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Double -> Int -> Int -> IO (Array DIM1 Double) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Float -> Int -> Int -> IO (Array DIM1 Float) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Word16 -> Int -> Int -> IO (Array DIM1 Word16) #-}
  {-# SPECIALIZE fromForeignPtr
    :: ForeignPtr Word32 -> Int -> Int -> IO (Array DIM1 Word32) #-}

  -- Allocate whole memory for writing, fill in with element of array.
  --
  toForeignPtr arr = do
    let sh = R.extent arr
        nelem = R.size sh
        dummy = sizeOf (undefined :: e)
    fptr <- mallocForeignPtrBytes (dummy * nelem)
    withForeignPtr fptr $ \ptr ->
      R.withManifest' arr $ \arr' ->
        let go i
              | i == nelem = return ()
              | otherwise  = pokeElemOff ptr i (arr' R.! (Z :. i)) >> go (i+1)
        in  go 0
    return (fptr, 0, nelem)

  {-# INLINE toForeignPtr #-}
  {-# SPECIALIZE toForeignPtr
    :: Array DIM1 Double -> IO (ForeignPtr Double, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array DIM1 Float -> IO (ForeignPtr Float, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array DIM1 Word16 -> IO (ForeignPtr Word16, Int, Int) #-}
  {-# SPECIALIZE toForeignPtr
    :: Array DIM1 Word32 -> IO (ForeignPtr Word32, Int, Int) #-}

-- | Unsafe from foreign pointer.
--
-- This function has introduced in repa 2.2.0.
-- Writing here again to support repa < 2.2.0.
--
unsafeFFP :: (Shape sh, Storable a) => sh -> ForeignPtr a -> Array sh a
unsafeFFP sh fptr =
  -- XXX: Using unsafePerformIO for peeking pointer.
  R.fromFunction sh $ \ix ->
    unsafePerformIO $ withForeignPtr fptr $ \ptr ->
      peekElemOff ptr $ R.toIndex sh ix
{-# INLINE unsafeFFP #-}

-- | Converts multi channel signal to vector signal.
fromMC :: Elt a => Array DIM2 a -> Array DIM1 a
fromMC arr = R.backpermute sh' f arr where
  sh' = Z :. (nc * nf)
  _ :. nc :. nf = R.extent arr
  f (Z :. i) = Z :. i `mod` nc :. i `div` nc
{-# INLINE fromMC #-}

-- | Converts vector signal to multi channel signal.
toMC :: Elt a => Int -> Array DIM1 a -> Array DIM2 a
toMC nc arr = R.backpermute sh' f arr where
  sh' = Z :. nc :. (nf `div` nc)
  _ :. nf = R.extent arr
  f (Z :. i :. j) = Z :. i + (j * nc)
{-# INLINE toMC #-}

{-
-- ---------------------------------------------------------------------------
-- Sample arrays

a0, a1, a2 :: Array DIM2 Int
a0 = R.fromList (Z :. 1 :. 10) [1..10]
a1 = R.fromList (Z :. 2 :. 5) [1..10]
a2 = R.fromList (Z :. 4 :. 5) [1..20]

a3,a4 :: Array DIM2 (Int,Int)
a3 = R.fromFunction (Z :. 2 :. 5) (\(_:.i:.j) -> (i,j))
a4 = R.fromFunction (Z :. 4 :. 5) (\(_:.i:.j) -> (i,j))
-}
