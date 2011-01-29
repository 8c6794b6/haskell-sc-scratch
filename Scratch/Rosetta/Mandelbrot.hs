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
-- * <http://rosettacode.org/wiki/Mandelbrot_set#Haskell>
--
module Main where

import Data.Complex

main :: IO ()
main = mapM_ putStrLn
 [[if magnitude (mandelbrot (x:+y)) < 2 then '*' else ' '
  | x <- [-2,-1.9685 .. 0.5]]
 | y <- [1, 0.95 .. -1]]

mandelbrot :: (Num a) => a -> a
mandelbrot a = iterate (\z -> z^2 + a) a !! 50