{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
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
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (Storable(..))
import Foreign.Marshal
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Array.Repa (Array, DIM1, DIM2, Elt(..), Shape(..), Z(..), (:.)(..))

import Sound.File.Sndfile
  (Buffer(..), Format, Info(..), Sample, SampleFormat(..))

import qualified Data.Array.Repa as R
import qualified Sound.File.Sndfile as S
import qualified Sound.File.Sndfile.Buffer.Vector as SV
import qualified Data.Vector.Storable as V

------------------------------------------------------------------------------
-- Wrapper

-- | Wrapper for sound file related actions.
newtype SF a = SF {unSF :: IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Performs sound file related action.
runSF :: SF a -> IO a
runSF = undefined

-- | Read audio file to array.
readSF :: FilePath -> SF (Array DIM2 Double)
readSF path = undefined

-- | Write array to sound file.
writeSF :: Info -> FilePath -> SF (Array DIM2 Double) -> IO ()
writeSF info path arr = undefined

------------------------------------------------------------------------------
-- Work

-- | Orphan instance for reading/wriging sound file to array via ForeignPtr.
--
instance (Storable e, Sample e, Elt e, Num e) => Buffer (Array DIM1) e where

  -- {-# INLINE fromForeignPtr #-}
  -- {-# INLINE toForeignPtr #-}

  -- Read the whole contents to DIM1 array, ignoring channel number.
  -- Using unsafePerformIO for peeking pointer.
  -- 
  fromForeignPtr fptr start size = return $ unsafeFFP (Z :. size) fptr

  -- Allocate memory and fill in with element of array.
  --
  toForeignPtr arr = do
    let sh = R.extent arr
        nelem = R.size sh
        dummy = sizeOf (undefined :: e)
    fptr <- mallocForeignPtrBytes (dummy * nelem)
    withForeignPtr fptr $ \ptr ->
      let go i | i == nelem = return ()
               | otherwise  = pokeElemOff ptr i (arr R.! (Z :. i)) >> go (i+1)
      in  go 0
    return (fptr, 0, nelem)
    
  -- toForeignPtr arr = R.withManifest' arr $ \arr' ->
  --   let sh = R.extent arr'
  --       nelem = R.size sh
  --       bit = sizeOf (undefined :: e)
  --   in  allocaBytes (bit * nelem) $ \ptr -> do
  --         pokeElemOff ptr nelem (arr' R.! (Z :. 0))
  --         fptr <- newForeignPtr finalizerFree ptr
  --         return $ (fptr, 0, nelem)
  --

    -- ptr <- mallocBytes (bit * nelem)
    -- pokeElemOff ptr nelem (arr' R.! (Z :. 0))
    -- fptr <- newForeignPtr_ ptr
    -- return (fptr, start, nelem)

    -- allocaBytes (bit * nelem) $ \ptr -> do
    --   pokeElemOff ptr nelem (arr' R.! (Z :. 0))
    --   fptr <- newForeignPtr finalizerFree ptr
    --   return $ (fptr, start, nelem)

    -- Poke elements in array to memory
    -- fptr <- mallocForeignPtrBytes (bit*nelem) :: IO (ForeignPtr e)
    -- withForeignPtr fptr $ \ptr -> do
    --   return $ R.traverse arr' id
    --      (\f ix -> unsafePerformIO $ pokeElemOff ptr (R.toIndex sh ix) (f ix))
    -- return (fptr, start, nelem)

  -- toForeignPtr arr = R.withManifest' arr $ \arr' -> do
  --   let sh = R.extent arr'
  --       bit = sizeOf (undefined :: e)
  --       nelem = R.size sh
        
  --   fptr <- mallocForeignPtrBytes (bit*nelem) :: IO (ForeignPtr e)
  --   withForeignPtr fptr $ \ptr -> do
  --     return $ R.traverse arr' id
  --       (\f ix -> unsafePerformIO $ pokeElemOff ptr (R.toIndex sh ix) (f ix))
  --   return (fptr, 0, nelem)
        
    -- allocaBytes (bit * nelem) $ \ptr -> do
    --   poke ptr (arr' R.! (Z :. 0))
    --   fptr <- newForeignPtr_ ptr
    --   return (fptr, 0, nelem)

-- Unsafe from foreign pointer, introduced from repa 2.2.0.
-- Writing manually to support repa < 2.2.0.
--      
unsafeFFP :: (Shape sh, Storable a) => sh -> ForeignPtr a -> Array sh a      
unsafeFFP sh fptr =
  R.fromFunction sh $ \ix ->
  unsafePerformIO $ withForeignPtr fptr $ \ptr ->
  peekElemOff ptr $ R.toIndex sh ix

------------------------------------------------------------------------------
-- Tests

test_read file = do
  let frames :: Info -> Int
      frames = S.frames
  (info, buf) <- S.readFile file :: IO (Info, Maybe (Array DIM1 Double))
  case buf of
    Nothing  -> error $ "test_read: Failed reading " ++ file
    Just arr -> return $ (info, R.reshape (Z :. S.channels info :. frames info) arr)

test_write arr info file = do
  let Z :. nchan :. nframe = R.extent arr
      arr' = R.reshape (Z :. R.size (R.extent arr)) arr
  S.writeFile (info {S.channels = nchan}) file arr'
  
sin440 :: Array DIM2 Double
sin440 =   
  let sh = Z :. 1 :. 4800 :: DIM2
  in  R.fromFunction sh $ \ix -> 
        sin (440 * fromIntegral (R.toIndex sh ix) * pi * 2/ 48000)
        
write_sin440 :: IO Int
write_sin440 = test_write sin440 waveMonoPcm32 "out.wav"

test_copy :: FilePath -> FilePath -> IO Int
test_copy ifile ofile = do
  (i,a) <- test_read ifile
  let a' = R.reshape (Z :. R.size (R.extent a)) a
  cnt <- S.writeFile i ofile a'
  return (cnt :: Int)

test_rw :: IO Int
test_rw = do
  (i,a) <- test_read "440hz_32bit_48000sr_1ch.wav"
  test_write a i "tmp.wav"

test_rw_880 :: IO Int
test_rw_880 = do
  (i,a) <- test_read "880hz_32bit_48000sr_1ch.wav"
  test_write a i "tmp.wav"

test_max :: FilePath -> IO (Array DIM1 Double)
test_max file = do
  (i, a) <- test_read file
  return $ R.fold max 0 a

test_read_vec file = do
  (i, res) <- S.readFile file
  case res of
    Nothing  -> error "Fail"
    Just sig -> return $ (i, SV.fromBuffer sig)

test_write_vec :: Sample a => Info -> FilePath -> V.Vector a -> IO ()
test_write_vec info file vec = do
  S.writeFile info file (SV.toBuffer vec)
  return ()

test_copy_vec ifile ofile = do
  (i, vec) <- test_read_vec ifile :: IO (Info, V.Vector Double)
  test_write_vec i ofile vec

waveMonoPcm32 :: Info
waveMonoPcm32 = S.defaultInfo
  { S.samplerate = 48000
  , S.channels = 1
  , S.format = S.Format S.HeaderFormatWav S.SampleFormatPcm32 S.EndianFile
  , S.sections = 1
  , S.seekable = True }

waveMonoPcm16 :: Info
waveMonoPcm16 = waveMonoPcm32
  {S.format = S.Format S.HeaderFormatWav S.SampleFormatPcm16 S.EndianFile }

waveMonoDouble :: Info
waveMonoDouble = waveMonoPcm32
  { S.format = S.Format S.HeaderFormatWav S.SampleFormatDouble S.EndianFile }

newtype RawList a = RawList {unRawList :: [a]}

read_rawlist :: FilePath -> IO ()
read_rawlist path = do
  (i,a) <- S.readFile path :: IO (Info, Maybe (RawList Double))
  case a of
    Nothing  -> error $ "read_raw: failed to read " ++ path
    Just sig -> print $ unRawList sig

instance Sample e => Buffer RawList e where
  fromForeignPtr fptr start sz =
    withForeignPtr fptr $ \ptr -> do
      sigs <- mapM (\i -> peekByteOff ptr (i*sizeOf (undefined :: e))) [0..sz-1]
      return $ RawList sigs
  toForeignPtr (RawList xs) = do
    return undefined

withHandle :: (S.Handle -> IO a) -> FilePath -> IO a
withHandle k path = do
  return undefined

read_raw :: FilePath -> IO [Double]
read_raw path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info
  ptr <- malloc :: IO (Ptr Double)
  
  let go p n acc
        | n == S.frames info = return acc
        | otherwise          = do 
          S.hGetBuf hdl p 1
          !v <- peek p
          go p (n+1) (v:acc)
          
  vs <- go ptr 0 []
  free ptr
  S.hClose hdl
  return $ reverse vs
  
read_raw_arr :: FilePath -> IO (Array DIM2 Double)  
read_raw_arr path = do
  info <- S.getFileInfo path
  hdl <- S.openFile path S.ReadMode info  
  ptr <- malloc :: IO (Ptr Double)
  
  let sh = Z :. 1 :. S.frames info :: DIM2
      go _ = do
        S.hGetBuf hdl ptr 1
        !v <- peek ptr
        return v
        
  arr <- return $ R.fromFunction sh $ \ix -> 
    unsafePerformIO $ unsafeInterleaveIO (go ix)
  free ptr
  S.hClose hdl
  return arr
  
write_raw :: FilePath -> [Double] -> IO ()
write_raw path vs = do
  hdl <- S.openFile path S.WriteMode (waveMonoPcm32 {frames = length vs})
  ptr <- malloc :: IO (Ptr Double)
  
  let bit = sizeOf (undefined :: Double)
      go p xs = case xs of
        []      -> return ()
        (!y:ys) -> poke p y >> S.hPutBuf hdl p 1 >> go p ys
        
  go ptr vs        
  free ptr
  S.hClose hdl
  
copy_raw :: FilePath -> FilePath -> IO ()
copy_raw ifile ofile =
  read_raw ifile >>= write_raw ofile

-- 0.7569950553588569,0.7181262974627316,0.6768759693950415,0.6333808721974492