{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Command line wrapper for 'lambda-calculus cooked four ways'.

-}
module Main where

import System.Environment

import Lambda
import IdInt
import qualified HOAS
import qualified Simple
import qualified Unique

main :: IO ()
main = do
  typ <- getArgs
  let f :: LC Id -> LC Id
      f e = e
      nf = case typ of
        ("H":_) -> HOAS.nf
        ("S":_) -> Simple.nf
        ("U":_) -> Unique.nf
        _       ->
          error $ "Invalid arg: " ++ concat typ ++ ", H, S or U expected."
  contents <- readFile "timing.lam"
  putStrLn . show . nf . toIdInt . f . read $ contents
