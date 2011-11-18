{-# LANGUAGE NoImplicitPrelude #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Benchmarks for data structures in RecSlowdown.

-}
module RecSlowdown.Bench where

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
  let mkappend n lr = bench n (whnf (uncurry (++)) lr)
      append_ops k =
        [ mkappend "ss" (mkShal k, mkShal k)
        , mkappend "sd" (mkShal k, mkDeep k)
        , mkappend "ds" (mkDeep k, mkShal k)
        , mkappend "dd" (mkDeep k, mkDeep k) ]

      mkuop st n f cat = bench n (st f cat)
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

  defaultMain
    [ bgroup "append"
      [ bgroup "n=100" (append_ops 100)
      , bgroup "n=1000" (append_ops 1000)
      , bgroup "n=10000" (append_ops 10000) ]
    , bgroup "tail"
      [ bgroup "n=100" (tail_ops 100)
      , bgroup "n=1000" (tail_ops 1000)
      , bgroup "n=10000" (tail_ops 10000) ]
    , bgroup "cons"
      [ bgroup "n=100" (cons_ops 100)
      , bgroup "n=1000" (cons_ops 1000)
      , bgroup "n=10000" (cons_ops 10000) ]
    , bgroup "snoc"
      [ bgroup "n=100" (snoc_ops 100)
      , bgroup "n=1000" (snoc_ops 1000)
      , bgroup "n=10000" (snoc_ops 10000) ]
    , bgroup "tolist"
      [ bgroup "n=100" (tolist_ops 100)
      , bgroup "n=1000" (tolist_ops 1000)
      , bgroup "n=10000" (tolist_ops 10000) ]
    ]
