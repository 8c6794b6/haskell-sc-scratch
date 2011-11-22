{-# LANGUAGE BangPatterns, TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable

Tests for reading and writing sound files.

-}
module Test.ReadWrite where

import Foreign.Ptr
import Foreign.Storable (Storable(..))
import Foreign.Marshal
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Data.Array.Repa ((:.)(..), Array(..), DIM2, Z(..))
import Sound.File.Sndfile (Sample(..))

import qualified Data.Array.Repa as R
import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as S
import qualified Sound.File.Sndfile.Buffer.Vector as SV

import Data.Array.Repa.IO.Sndfile

-- ---------------------------------------------------------------------------
-- Read sound file as repa array, then write it without modification.

test_rw :: FilePath -> FilePath -> IO ()
test_rw i o = do
  (info,arr) <- readSF i :: IO (Info, Array DIM2 Double)
  writeSF o info arr


-- ---------------------------------------------------------------------------
-- Using storable vector, much faster.

test_read_vec :: Sample a => FilePath -> IO (Info, V.Vector a)
test_read_vec file = do
  (i, res) <- S.readFile file
  case res of
    Nothing  -> error "Fail"
    Just sig -> return $ (i, SV.fromBuffer sig)

test_write_vec :: Sample a => Info -> FilePath -> V.Vector a -> IO ()
test_write_vec info file vec = do
  S.writeFile info file (SV.toBuffer vec)
  return ()

test_copy_vec :: FilePath -> FilePath -> IO ()
test_copy_vec ifile ofile = do
  (i, vec) <- test_read_vec ifile :: IO (Info, V.Vector Double)
  test_write_vec i ofile vec


-- ---------------------------------------------------------------------------
-- Raw operations

read_raw :: FilePath -> IO (Info, [Double])
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
  return $ (info, reverse vs)

read_raw_arr :: FilePath -> IO (Info, Array DIM2 Double)
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
  return (info, arr)

write_raw :: FilePath -> Info -> [Double] -> IO ()
write_raw path info vs = do
  hdl <- S.openFile path S.WriteMode info
  ptr <- malloc :: IO (Ptr Double)

  let go p xs = case xs of
        []      -> return ()
        (!y:ys) -> poke p y >> S.hPutBuf hdl p 1 >> go p ys

  go ptr vs
  free ptr
  S.hClose hdl

copy_raw :: FilePath -> FilePath -> IO ()
copy_raw ifile ofile = do
  (info, arr) <- read_raw ifile
  write_raw ofile info arr
