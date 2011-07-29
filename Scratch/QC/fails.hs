module Main where

import System.Exit
import Test.QuickCheck

main :: IO ()
main = do
  results <- mapM quickCheckResult 
    [label "prop_success" prop_success
    ,label "prop_fail" prop_fail
    ,label "prop_success'" prop_success']
  if any (not . isSuccess) results 
    then exitFailure 
    else exitSuccess
  
isSuccess :: Result -> Bool
isSuccess r = case r of Success _ _ _ -> True; _ -> False

prop_fail :: Int -> Property
prop_fail x = x > 10 ==> x < 20

prop_success :: Int -> Property
prop_success x = x > 10 ==> x > 9

prop_success' :: Int -> Property
prop_success' x = x > 5 ==> x > 4