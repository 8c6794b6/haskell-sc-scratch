{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Read and write audio file with repa arrays using libsndfile via hsndfile.

-}
module Data.Array.Repa.IO.Sndfile where

import Control.Applicative
import Data.Word (Word16, Word32)
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))
import Foreign.Marshal
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Array.Repa (Array, DIM1, DIM2, Elt(..), Shape(..), Z(..), (:.)(..))

import Sound.File.Sndfile
  (Buffer(..), Format, Info(..), Sample, SampleFormat(..))

import qualified Data.Array.Repa as R
import qualified Sound.File.Sndfile as S

------------------------------------------------------------------------------
-- Wrapper

-- | Wrapper for sound file related actions.
newtype SF a = SF {unSF :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Performs sound file related action.
runSF :: SF a -> IO a
runSF = undefined

-- | Read audio file to array.
readAudio :: FilePath -> SF (Array DIM2 Double)
readAudio path = undefined

-- | Write array to sound file.
writeAudio :: Info -> FilePath -> SF (Array DIM2 Double) -> IO ()
writeAudio info path arr = undefined

------------------------------------------------------------------------------
-- Work

-- | Orphan instance for reading/wriging sound file to array via ForeignPtr.
--
instance (Storable e, Sample e, Elt e, Num e) => Buffer (Array DIM1) e where

  -- Read the whole contents to DIM1 array, ignoring channel number.
  -- Using unsafePerformIO for peeking pointer.
  --
  fromForeignPtr fptr start size = withForeignPtr fptr $ \ptr ->
    return $ R.fromFunction (Z :. size) $ \(_ :. i) ->
      unsafePerformIO $ peekElemOff ptr (i * sizeOf (undefined :: e))

  -- Allocate memory, then point the first element of array.
  --
  toForeignPtr arr = R.withManifest' arr $ \arr' ->
    let start = 0
        nelem = R.size $ R.extent arr'
        bit = alignment (undefined :: e)
    in  allocaBytes (bit * nelem) $ \ptr -> do
          pokeElemOff ptr nelem (arr' R.! (Z :. 0))
          fptr <- newForeignPtr_ ptr
          return $ (fptr, start, nelem)

-- | Get bit depth from sample format.
--
-- This function is doing a rough gess, may not get intended result.
-- Returns 16 when the bit depth of given format is unsure.
--
bitDepth :: SampleFormat -> Int
bitDepth fmt = case fmt of
  --
  -- XXX: Unreliable.
  -- General rule to convert bit width from file is by normalisation,
  -- though currently this feature is missing in haskell libsndfile binding.
  -- (hsndfile-0.5).
  --
  SampleFormatPcmS8       -> 8
  SampleFormatPcm24       -> 24
  SampleFormatPcm32       -> 32
  SampleFormatPcmU8       -> 8
  SampleFormatDouble      -> 32  -- XXX: We cannot assume double as 32bit.
  SampleFormatG72132      -> 32
  SampleFormatG72324      -> 16
  SampleFormatG72340      -> 40
  SampleFormatDwvw12      -> 12
  SampleFormatDwvw24      -> 24
  SampleFormatFormatDpcm8 -> 8
  _                       -> 16


------------------------------------------------------------------------------
-- Tests

-- Reads 32bit WAVE file.
test_read file = do
  info <- S.getFileInfo file
  let depth = bitDepth $ S.sampleFormat $ S.format info
      frames :: Info -> Int
      frames = S.frames
  (_, buf) <- S.readFile file :: IO (Info, Maybe (Array DIM1 Word32))
  case buf of
    Nothing  -> error $ "Failed reading " ++ file
    Just arr -> return $ R.reshape (Z :. S.channels info :. frames info) arr

test_write ifile ofile = do
  info <- S.getFileInfo ifile
  a <- test_read ifile
  let a' = R.reshape (Z :. R.size (R.extent a)) a
  cnt <- S.writeFile waveMonoPCM32 ofile a'
  return (cnt :: Int)

get_right_chan file = do
  return undefined

waveMonoPCM32 :: Info
waveMonoPCM32 = S.defaultInfo
  { S.samplerate = 48000
  , S.channels = 1
  , S.format = S.Format S.HeaderFormatWav S.SampleFormatPcm32 S.EndianLittle
  , S.sections = 1
  , S.seekable = True }