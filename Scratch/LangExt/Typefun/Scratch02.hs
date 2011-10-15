{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading typefun.pdf.
-}
module Scratch02 where

import Data.IORef
import Data.STRef
import Control.Monad.ST

class Mutation m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()

-- NG, type of r needs manual assistance.

instance Mutation IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance Mutation (ST s) (STRef s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

{-
    No instance for (Mutation IO r0)
      arising from a use of `readRef'
    Possible fix: add an instance declaration for (Mutation IO r0)
    In a stmt of a 'do' expression: y <- readRef r
    In the expression:
      do { r <- newRef 'x';
           y <- readRef r;
           print y }
    In an equation for `readAndPrint':
        readAndPrint
          = do { r <- newRef 'x';
                 y <- readRef r;
                 print y }

readAndPrint = do
  r <- newRef 'x'
  y <- readRef r
  print y

-}

-- Need an explicit type.
readAndPrint = do
  r <- newRef 'x' :: IO (IORef Char)
  y <- readRef r
  print y
