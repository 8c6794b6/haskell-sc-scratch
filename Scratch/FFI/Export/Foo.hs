{-# LANGUAGE ForeignFunctionInterface #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Example from:
--
-- * http://www.haskell.org/ghc/docs/7.0.2/html/users_guide/ffi-ghc.html
--
module Foo where

foreign export ccall foo :: Int -> IO Int

foo :: Int -> IO Int
foo n = return (length (f n))

f :: Int -> [Int]
f 0 = []
f n = n : (f (n-1))