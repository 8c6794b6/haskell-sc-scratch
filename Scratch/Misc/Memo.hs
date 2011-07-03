------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with memoized factorial, from:
--
-- * <http://d.hatena.ne.jp/ranha/20080709/1215608401>
--
module Memo where

main :: IO ()

-- real   0m9.224s
-- user   0m9.153s
-- sys    0m0.033s
-- main = exe1 10000

-- real   0m1.017s
-- user   0m1.010s
-- sys    0m0.003s
main = exe2 10000

-- real   0m0.014s
-- user   0m0.013s
-- sys    0m0.000s
-- main = exe1 100


fact n = foldl (*) 1 [1..n]

newtype Memo = Memo {test::Integer}

gen = Memo . fact

exe1 n = mapM_ (\x -> print $ fact n) [0..99]
exe2 n = mapM_ (\x -> print $ fact' n) [0..99] where
  fact' n = if n == 10000 then
              test m
            else
              fact n
  m = gen 10000
