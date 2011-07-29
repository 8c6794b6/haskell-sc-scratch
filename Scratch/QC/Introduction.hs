module QC.Introduction where

import Data.Char
import Test.QuickCheck

getList :: IO String
getList = fmap take5 getContents

take5 :: String -> String
take5 = take 5 . filter (`elem` ['a'..'e'])

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs {maxSuccess=10000})