{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (TemplateHaskell)

Attempt to extract argument string passed to function with th.
Caller side of Template haskell code.

-}
module FunArgRun where

import FunArgSrc

main :: IO ()
main = do
  putStrLn $ unwords $ ["Argument of", show 'foo ++ ":"] ++ $(funArgs 'foo)
  putStrLn $ unwords $ ["Argument of", show 'bar ++ ":"] ++ $(funArgs 'bar)

foo :: Int -> Int -> Int -> Int                
foo apple banana cherry = (apple + banana) * cherry

bar :: String
bar = "This function does not take arguments."

thisLocation = $myLocation
