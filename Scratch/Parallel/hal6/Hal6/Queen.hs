{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable
-}
module Hal6.Queen where

import System.Environment

main :: IO ()
main = do
  (n:_) <- getArgs
  print $ length $ queens $ read n

type Solution = PartialSolution
type PartialSolution = [Int]
type BoardSize = Int

queens :: BoardSize -> [Solution]
queens n = iterate (concatMap (addQueen n)) [[]] !! n

addQueen :: BoardSize -> PartialSolution -> [PartialSolution]
addQueen n s = [x:s | x <- [1..n], safe x s 1]

safe :: Int -> PartialSolution -> Int -> Bool
safe x xs n = case xs of
  []    -> True
  (c:y) -> x /= c && x /= (c+n) && x /= (c-n) && safe x y (n+1)
