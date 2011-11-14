{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Attempt to extract argument string passed to function with th.
Runner side of Template haskell code.

-}
module FunArgRun where

import FunArgSrc

main :: IO ()
main = putStrLn $ unwords $
  ["Argument of", show 'foo ++ ":"] ++ $(funArgs 'foo)

foo :: Int -> Int -> Int -> Int                
foo apple banana cherry = (apple + banana) * cherry


thisLoc = $myLocation
thisContents = $myContents
thisModule = $myModule

