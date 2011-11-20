{-# LANGUAGE TypeOperators #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading:

 * http://haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial

-}
module Tut.Repa where

import Data.Word
import Foreign.Ptr
import System.Environment (getArgs)

import Criterion.Main
import Data.Array.Repa ((:.)(..), (!), (!?))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Algorithms.Randomish as R
import qualified Data.Array.Repa.ByteString as R
import qualified Data.Array.Repa.IO.Matrix as R
import qualified Data.Array.Repa.IO.BMP as R
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

------------------------------------------------------------------------------
--
-- * Fundamental concepts
--

-- Dimension base
z :: R.Z
z = R.Z

-- 3x3 dimension.
d3x3 :: R.DIM3
d3x3 = z :. 3 :. 3 :. 3

-- 3x3 dimension array.
x :: R.Array R.DIM3 Int
x = R.fromList d3x3 [1..27]

-- Array information.
x_extent :: R.DIM3
x_extent = R.extent x

-- Rank.
x_rank :: Int
x_rank = R.rank x_extent

-- Size.
x_size :: Int
x_size = R.size x_extent

-- Converted to unboxed vector.
x_vector :: U.Vector Int
x_vector = R.toVector x

------------------------------------------------------------------------------
--
-- * Building arrays
--

-- Building with function in repa package.
--
-- Input data used to create array.
--
inputs :: [Double]
inputs = [1..12]

-- One dimensional array of length 10, given the shape (Z :. 10)
--
-- > ghci> a01
-- > Array (Z :. 10) [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
--
a01 :: R.Array R.DIM1 Double
a01 = R.fromList (z :. 12) inputs

-- Feeding same input data t 2-dimensional array (6x2).
--
-- > ghci> a02
-- > Array (Z :. 6 :. 2) [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
--
a02 :: R.Array R.DIM2 Double
a02 = R.fromList (z :. 6 :. 2) inputs

-- And 3 dimensional array (2x3x2).
-- > ghci> a03
-- > Array (Z :. 2 :. 3 :. 2) [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
--
a03 :: R.Array R.DIM3 Double
a03 = R.fromList (z :. 2 :. 3 :. 2) inputs

-- Building from unboxed vector, one dimensional.
--
-- > ghci> a04
-- > Array (Z :. 10) [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]
--
a04 :: R.Array R.DIM1 Double
a04 = R.fromVector (z :. 10) (U.enumFromN 0 10)

-- Building from unboxed vector, two dimensional (3x3).
--
a05 :: R.Array R.DIM2 Double
a05 = R.fromVector (z :. 3 :. 3) (U.enumFromN 0 9)

-- Generating random arrays with repa-algorithms package.
-- Making 3 dimensional array (3x3x3) filled with random Int between 0 to 255.
-- The last arg (0xfacaba) passed to `randomishIntArray` is a seed used
-- for generating random values.
--
-- > ghci> a06
-- > Array (Z :. 3 :. 3 :. 3)
-- >   [ 150,26,143,  22,84,131,  92,199,81
-- >   , 180,245,211, 111,200,84, 152,196,124
-- >   , 226,8,154,   49,40,69,   35,221,26 ]
--
a06 :: R.Array R.DIM3 Int
a06 = R.randomishIntArray (z :. 3 :. 3 :. 3) 0 255 0xfacaba

------------------------------------------------------------------------------
--
-- Reading and writing
--

-- Writing a02 to file "test.dat". Using 'writeMatrixToTextFile' from
-- repa-io package. The contents of test.dat looks like this:
--
-- > $ cat test.dat
-- > MATRIX
-- > 6 2
-- > 1.0
-- > 2.0
-- > 3.0
-- > 4.0
-- > 5.0
-- > 6.0
-- > 7.0
-- > 8.0
-- > 9.0
-- > 10.0
-- > 11.0
-- > 12.0
--
write_a02 :: IO ()
write_a02 = R.writeMatrixToTextFile "test.dat" a02

-- Reading back from file to array.
--
read_a02 :: IO (R.Array R.DIM2 Double)
read_a02 = R.readMatrixFromTextFile "test.dat"

-- Reading from image file in bmp format.
--
read_bmp :: IO ()
read_bmp = do
  res <- R.readImageFromBMP "tile.bmp"
  case res of
    Left err  -> error $ show err
    Right img -> do
      putStrLn $ "Read bmp to Array.\nExtent: " ++ show (R.extent img)
      R.writeImageToBMP "copied.bmp" img
      putStrLn "Wrote copy to cpied.bmp"

------------------------------------------------------------------------------
-- Copying arrays from pointers.

i, j, k :: Int
[i, j, k] = [255, 255, 4]

v :: S.Vector Word8
v = S.fromList . take (i * j * k) . cycle $ concat
  [[r, g, b, 255] | r <- [0..255], g <- [0..255], b <- [0..255]]

ptr2repa :: Ptr Word8 -> IO (R.Array R.DIM3 Word8)
ptr2repa p = R.copyFromPtrWord8 (z :. i :. j :. k) p

write_grad :: IO ()
write_grad = do
  r <- S.unsafeWith v ptr2repa
  R.writeImageToBMP "test.bmp" r

-- Alternave: Using unboxed array

uv :: U.Vector Word8
uv = U.fromList . take (i*j*k) . cycle $ concat
  [[r,g,b,255] | r <- [0..255], g <- [0..255], b <- [0..255]]

write_grad' :: FilePath -> IO ()
write_grad' path = do
  let r = R.fromVector (z :. i :. j :. k) uv
  R.writeImageToBMP path r


------------------------------------------------------------------------------
-- Maps, zips, filters and folds

-- fold reduces the inner dimmension of the array.
-- a05 has DIM2 type.
--
a05_sum :: R.Array R.DIM1 Double
a05_sum = R.fold (+) 0 a05

-- Folding over to get sum of 3x3x3 array makes 3x3 array.
--
a03_sum :: R.Array R.DIM2 Double
a03_sum = R.fold (+) 0 a03

-- Combining arrays with zipWith
--
a05_zipped :: R.Array R.DIM2 Double
a05_zipped = R.zipWith (*) a05 a05

-- Array with different dimensions cannot zipped.
--
-- > ghci> R.zipWith (*) a05 a03
-- >
-- > <interactive>:1:19:
-- >     Couldn't match expected type `R.Z' with actual type `R.DIM0 :. Int'
-- >     Expected type: R.Array R.DIM2 Double
-- >       Actual type: R.Array R.DIM3 Double
-- >     In the third argument of `R.zipWith', namely `a03'
-- >     In the expression: R.zipWith (*) a05 a03
--
-- When dimension matches, shape does not necessary to be identical
--
-- > ghci> R.extent a03
-- > ((Z :. 2) :. 3) :. 2
-- > ghci> R.extent a06
-- > ((Z :. 3) :. 3) :. 3
-- > ghci> R.zipWith (*) (R.map fromIntegral a06) a03
-- > Array (Z :. 2 :. 3 :. 2)
-- >  [150.0,52.0,66.0,336.0,460.0,1194.0,1260.0,1960.0,999.0,2000.0,1672.0,2352.0]
--

-- traverse
--
a07 :: R.Array R.DIM3 Int
a07 = R.fromList (z :. 3 :. 3 :. 3) [1..27]

-- Replacing each element with 3rd dimension index.
--
-- > ghci> a07_trv1
-- > Array (Z :. 3 :. 3 :. 3)
-- >   [0,0,0,0,0,0,0,0,0
-- >   ,1,1,1,1,1,1,1,1,1
-- >   ,2,2,2,2,2,2,2,2,2]
--
a07_trv1 :: R.Array R.DIM3 Int
a07_trv1 = R.traverse a07 id $ \_ (R.Z :. i :. j :. k) -> i

-- Replacing each element with looking up different element
--
-- > ghci> a07_trv2
-- > Array (Z :. 3 :. 3 :. 3)
-- >   [1,4,7,10,13,16,19,22,25
-- >   ,2,5,8,11,14,17,20,23,26
-- >   ,3,6,9,12,15,18,21,24,27]
--
a07_trv2 :: R.Array R.DIM3 Int
a07_trv2 = R.traverse a07 id $ \f (R.Z :. i :. j :. k) -> f (z :. j :. k :. i)


------------------------------------------------------------------------------
-- Changing the shape of an array

transpose2D :: R.Elt e => R.Array R.DIM2 e -> R.Array R.DIM2 e
transpose2D a = R.backpermute (swap e) swap a where
  e = R.extent a
  swap (R.Z :. i :. j) = z :. j :. i


------------------------------------------------------------------------------
-- Example: rotating an image with backpermute

rot180 :: R.Array R.DIM3 Word8 -> R.Array R.DIM3 Word8
rot180 g = R.backpermute e flop g where
  e@(R.Z :. x :. y :. _) = R.extent g
  flop (R.Z :. i :. j :. k) = (z :. x-i-1 :. y-j-1 :. k)

rotimg :: FilePath -> FilePath -> IO ()
rotimg inf outf = do
  res <- R.readImageFromBMP inf
  case res of
    Left err -> error $ show err
    Right img -> R.writeImageToBMP outf (rot180 img)

------------------------------------------------------------------------------
-- Example: matrix multiplication

mmMult ::
  (Num e, R.Elt e) => R.Array R.DIM2 e -> R.Array R.DIM2 e -> R.Array R.DIM2 e
mmMult a b = R.sum (R.zipWith (*) aRepl bRepl) where
  aRepl = R.extend (z :. R.All :. colsB :. R.All) a
  bRepl = R.extend (z :. rowsA :. R.All :. R.All) t
  t = transpose2D b
  (R.Z :. colsA :. rowsA) = R.extent a
  (R.Z :. colsB :. rowsB) = R.extent b

--
-- mult_01 :
--
-- a = [ 5 6 ]
--     [ 7 8 ]
--
-- b = [ 1 2 ]
--     [ 3 4 ]
--
-- a * b =  [ (5*1 + 6*3) (5*2 + 6*4) ] = [ 23 34 ]
--          [ (7*1 + 8*3) (7*2 + 8*4) ]   [ 31 46 ]
--
mult_01 :: R.Array R.DIM2 Int
mult_01 = a `mmMult` b where
  a = R.fromList d2x2 [5,6,7,8]
  b = R.fromList d2x2 [1,2,3,4]
  d2x2 = z :. 2 :. 2

------------------------------------------------------------------------------
-- Example: parallel image desaturation

luminate :: FilePath -> FilePath -> IO ()
luminate inf outf = do
  r <- R.readImageFromBMP inf
  case r of
    Left err  -> error $ show err
    Right img ->
      let img' = R.traverse img id luminosity
      in  R.writeImageToBMP outf img'
{-# INLINEABLE luminate #-}

luminosity :: (R.DIM3 -> Word8) -> R.DIM3 -> Word8
luminosity f (R.Z :. i :. j :. k)
  | k == 3    = 255
  | otherwise = ceiling $ 0.21 * r + 0.71 * g + 0.07 * b
  where
    {-# INLINEABLE r #-}
    r = fromIntegral $ f (z :. i :. j :. 0)
    {-# INLINEABLE g #-}
    g = fromIntegral $ f (z :. i :. j :. 1)
    {-# INLINEABLE b #-}
    b = fromIntegral $ f (z :. i :. j :. 2)
{-# INLINEABLE luminosity #-}

-- Benchmarking forced and non-forced array operation.
-- Currently not showing difference. Find out when we need explicit forcing.
bench_para :: IO ()
bench_para = do
  let a :: R.Array R.DIM2 Int
      !a = R.fromList (z :. 1024 :. 1024) [1..1024*1024]
  defaultMain
    [ bgroup "bench"
      [ bench "force" (whnf sum_force a)
      , bench "no-force" (whnf sum_no_force a)
      ]
    ]

sum_force :: R.Shape sh => R.Array (sh :. Int) Int -> R.Array sh Int
sum_force = R.force . R.fold (+) 0

sum_no_force :: R.Shape sh => R.Array (sh :. Int) Int -> R.Array sh Int
sum_no_force = R.fold (+) 0