{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Exception used commonly.

-}
module Tree.Exception where

import Control.Exception
import Data.Typeable

data TreeException
   = EmptyTreeException
   | TreeIndexOutOfRangeException
   deriving (Show, Typeable)

emptyTreeException :: a
emptyTreeException = throw EmptyTreeException

treeIndexOutOfRangeException :: a
treeIndexOutOfRangeException = throw TreeIndexOutOfRangeException

instance Exception TreeException