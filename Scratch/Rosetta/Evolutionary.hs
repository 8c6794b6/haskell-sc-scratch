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
-- * <http://rosettacode.org/wiki/Evolutionary_algorithm#Haskell>
--
module Main where

import Control.Arrow
import Control.Monad
import Data.Array
import Data.List
import Data.Ord
import System.Random

main = origin >>= coverge 0

coverge n parent = do
  when (n `mod` printEvery == 0) (putStrLn fmtd)
  if target == parent
     then putStrLn $ "\nFinal: " ++ fmtd
     else mapM mutate (replicate (popSize-1) parent) >>=
          coverge (n+1) . fst . maximumBy (comparing snd) .
          map (id &&& fitness) . (parent:)
  where
    fmtd = parent ++ ": " ++ show (fitness parent) ++ " (" ++ show n ++ ")"

mutate = mapM $ \c -> do
  r <- randomRIO (0,1.0::Double)
  if r < mutateRate then randomChar else return c

fitness = length . filter id . zipWith (==) target

origin = mapM (\_ -> randomChar) target

randomChar = randomRIO (0,26::Int) >>= return . (alphabet !)

alphabet = listArray (0,26) (' ':['A'..'Z'])
target = "METHINKS IT IS LIKE A WEASEL"
mutateRate = 0.1
popSize = 100
printEvery = 5