{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch written while reading
/purely functional data structure/, by Chris Okasaki.

This codes contains physicist's method implementation of Queue shown in chapter 3.

-}
module Queue.Physicist where

import Prelude hiding (head, tail)
import qualified Prelude

import Queue.Initial (emptyQueueException)

------------------------------------------------------------------------------
-- * Physicists Queue
--
-- Invariants: 
--   W is a prefix of fourceW, W = [] only if F = [],
--   |F| >= |R|, LenF = |F|, LenR = |R|
--
data PQueue a = PQueue {}