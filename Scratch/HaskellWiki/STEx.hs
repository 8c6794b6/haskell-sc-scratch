------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (GHC specific)
--
-- Playing with ST monad.
--
module STEx where

import Control.Monad
import Control.Monad.ST
import Data.STRef

sumST :: Num a => [a] -> a
sumST xs = runST $ do 
  n <- newSTRef 0
  forM_ xs $ \x -> do
    modifySTRef n (+x)
  readSTRef n
  
sumST' xs = do   
  n <- newSTRef 0
  forM_ xs $ \x -> modifySTRef n (+x)
  readSTRef n

foldlST :: (a->b->a) -> a -> [b] -> a
foldlST z v xs = runST $ do
  v' <- newSTRef v
  forM_ xs $ \x -> do
    a <- readSTRef v'
    writeSTRef v' (z a x)
  readSTRef v'
  
fibST n =   
  if n < 2 
    then n 
    else runST $ do
      x <- newSTRef 0
      y <- newSTRef 1
      fibST' n x y
      
fibST' 0 x _ = readSTRef x
fibST' n x y = do
  x' <- readSTRef x
  y' <- readSTRef y
  writeSTRef x y'
  writeSTRef y (x'+y')
  fibST' (n-1) x y