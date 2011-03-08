{-# LANGUAGE Arrows #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Playing with arrows.
--
module A1 where

import Control.Arrow
import Sound.SC3

addA f g = proc x -> do
  y <- f -< x
  z <- g -< x
  returnA -< y + z

-- foo = do 
--   let s = sinOsc ar
--   0 => phase s
--   lfdNoise3 kr 8 => freq s
--   return $ out 0 s
  
-- a1 = \f -> sinOsc ar f 0 >>^ out 0 
