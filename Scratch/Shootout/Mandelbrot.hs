{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -O2 -funbox-strict-fields -fvia-C -optc-O2 -fexcess-precision #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- * <http://shootout.alioth.debian.org/u64/program.php?test=mandelbrot&lang=ghc&id=2>
--
module Main where

import System
import System.IO
import Foreign
import Foreign.Marshal.Array
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad

main :: IO ()
main = do
  w <- getArgs >>= readIO . head
  let n = w `div` 8
      m = 2 / fromIntegral w
      coords = [T 1 0 y (fromIntegral y * m - 1) | y <- [0..w]]
  q <- newChan
  replies <- replicateM w newEmptyMVar
  mapM_ (writeChan q) $ zip coords replies
  replicateM_ 4 . forkIO $ worker q w m n
  putStrLn ("P4\n"++show w++" "++show w)
  mapM_ (takeMVar >=> \b -> hPutBuf stdout b n) replies

worker :: Chan (T, MVar (Ptr Word8)) -> Int -> Double -> Int -> IO ()
worker q w m n = forever $ do
  (coord, reply) <- readChan q
  p <- mallocArray0 n
  unfold (next_x w m n) p coord
  putMVar reply p

unfold :: (T -> Maybe (Word8,T)) -> Ptr Word8 -> T -> IO (Ptr Word8)
unfold !f !ptr !x0 = go ptr x0
  where
    go !p !x = case f x of
      Just (w,y) -> poke p w >> go (p `plusPtr` 1) y
      Nothing    -> return ptr

data T = T !Int !Int !Int !Double

next_x :: (Bits t) => Int -> Double -> Int -> T -> Maybe (t, T)
next_x !w !iw !bw (T bx x y ci)
  | bx == bw  = Nothing
  | otherwise = Just (loop_x w x 8 iw ci 0, T (bx+1) (x+8) y ci)

loop_x :: (Fractional b, Ord b, Bits a1, Integral a) =>
          a -> a -> Int -> b -> b -> a1 -> a1
loop_x !w !x !n !iw !ci !b
  | x < w     = if n == 0 then b else loop_x w (x+1) (n-1) iw ci (b+b+v)
  | otherwise = b `shiftL` n
  where
    v = fractal 0 0 (fromIntegral x * iw - 1.5) ci 50

fractal :: (Num a, Ord a, Num t, Num a1) => a -> a -> a -> a -> a1 -> t
fractal !r !i !cr !ci !k
  | r2 + i2 > 4 = 0
  | k == 0      = 1
  | otherwise   = fractal (r2-i2+cr) ((r+r)*i+ci) cr ci (k-1)
  where
    (!r2,!i2) = (r*r,i*i)
