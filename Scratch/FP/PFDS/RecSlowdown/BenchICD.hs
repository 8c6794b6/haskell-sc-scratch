{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Benchmarks for data structures in RecSlowdown.

-}
module RecSlowdown.BenchICD where

import Prelude hiding (head, tail, last, init, (++))
import RecSlowdown.ICD
import Criterion.Main

rep :: (a -> a) -> a -> Int -> a
rep f v n
  | n == 0    = v
  | otherwise = let x = rep f v (n-1) in x `seq` f x

mkShal :: Int -> Cat Int
mkShal len = rep (cons 0) empty len

mkDeep :: Int -> Cat Int
mkDeep len = rep (shl ++) empty len where
  shl  = mkShal len'
  len' = 8

main :: IO ()
main = do
  let mkappend n !lr = bench n (whnf (uncurry (++)) lr)
      append_ops k =
        [ mkappend "ss" (mkShal k, mkShal k)
        , mkappend "sd" (mkShal k, mkDeep k)
        , mkappend "ds" (mkDeep k, mkShal k)
        , mkappend "dd" (mkDeep k, mkDeep k) ]

      mkuop st n f !cat = bench n (st f cat)
      tail_ops k =
        [ mkuop whnf "s" tail (mkShal k)
        , mkuop whnf "d" tail (mkDeep k) ]
      cons_ops k =
        [ mkuop whnf "s" (cons 1) (mkShal k)
        , mkuop whnf "d" (cons 1) (mkDeep k) ]
      snoc_ops k =
        [ mkuop whnf "s" (snoc 1) (mkShal k)
        , mkuop whnf "d" (snoc 1) (mkDeep k) ]
      tolist_ops k =
        [ mkuop nf "s" toList (mkShal k)
        , mkuop nf "d" toList (mkDeep k) ]

      mkgroup (n,f) =
        bgroup n
          [ bgroup "n=100" (f 100)
          , bgroup "n=1000" (f 1000)
          , bgroup "n=10000" (f 10000) ]

  defaultMain $ map mkgroup
    [ ("append", append_ops)
    , ("tail", tail_ops)
    , ("cons", cons_ops)
    , ("snoc", snoc_ops)
    , ("tolist", tolist_ops) ]
