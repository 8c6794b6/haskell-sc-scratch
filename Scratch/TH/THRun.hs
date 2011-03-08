{-# LANGUAGE TemplateHaskell #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with Template Haskell, printf runner.
--
module THRun where

import THSrc

main = do
  putStr printfR
  putStr selR
  
printfR :: String
printfR = $(printf "Hello, %d, %s, %d \n") 100 "world" True

selR :: String
selR = unwords ["last element of", show t, "is", show ($(sel 5 5) t), "\n"]
  where
    t = ('a', False, 1, [100,200,300], "foo")
    
    
infoR = $(infoExample "zipN")