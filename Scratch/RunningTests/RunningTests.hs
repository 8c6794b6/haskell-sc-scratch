module Main where

import Data.List (sort, sortBy)
import Test.QuickCheck ((==>))
import qualified Test.QuickCheck as QC

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.Framework as TF

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = 
  [testGroup "Sorting Group 1" 
   [testProperty "sort1" prop_sort1
   ,testProperty "sort2" prop_sort2]
  ,testGroup "Sorting Group 2"
   [testProperty "sort3" prop_sort3]]
        
prop_sort1 :: [Int] -> Bool
prop_sort1 xs = sort xs == sortBy compare xs
  
prop_sort2 :: [Int] -> QC.Property
prop_sort2 xs = (not (null xs)) ==> (head (sort xs) == minimum xs)

prop_sort3 :: [Int] -> QC.Property
prop_sort3 xs = (not (null xs)) ==> last (sort xs) == maximum xs
