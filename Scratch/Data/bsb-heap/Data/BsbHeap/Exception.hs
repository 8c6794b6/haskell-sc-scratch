{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

Exceptions used thrown from heap.

-}
module Data.BsbHeap.Exception where

import Control.Exception
import Data.Typeable

-- | Exception used thrown during operation of heap.
data HeapException
  = EmptyHeap           -- ^ Heap is empty
  | TreeIndexOutOfRange -- ^ Invalid indexing in internal tree
  deriving (Show, Typeable)
           
instance Exception HeapException
           
emptyHeap :: a
emptyHeap = throw EmptyHeap

treeIndexOutOfRange :: a
treeIndexOutOfRange = throw TreeIndexOutOfRange