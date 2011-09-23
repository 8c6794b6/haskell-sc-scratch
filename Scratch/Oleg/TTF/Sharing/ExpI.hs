{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/ExpI.hs>

-}
module ExpI where

import BiMap

data Exp 
  = Add Exp Exp
  | Variable String
  | Constant Int
  deriving (Eq, Ord, Show)
           
exp_a = Add (Constant 10) (Variable "i1")           
exp_b = Add exp_a (Variable "i2")

mul :: Int -> Exp -> Exp
mul n x = case (n,x) of
  (0,_) -> Constant 0
  (1,_) -> x
  _      | even n    -> mul (n `div` 2) (Add x x)
         | otherwise -> Add x (mul (n-1) x)
                        
exp_mul4 = mul 4 (Variable "i1")
exp_mul8 = mul 8 (Variable "i1")

sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky f xs = case xs of
  []  -> []
  [y] -> xs
  _   -> left' ++ [f (last left') r | r <- right' ]
  where
    (left,right) = splitAt (length xs `div` 2) xs
    left'  = sklansky f left
    right' = sklansky f right
  
test_sklansky_i n = sklansky Add xs
  where xs = Prelude.map (Variable . show) [1..n]
        
-- Directed Acyclic Graph?
--
newtype DAG = DAG (BiMap Exp) deriving Show

exp_to_dag :: Exp -> DAG -> DAG
exp_to_dag e d@(DAG m) | Just _ <- lookup_key e m = d
exp_to_dag e@(Add e1 e2) d = 
  let d1 = exp_to_dag e1 d
      d2 = exp_to_dag e2 d1
      DAG m2 = d2
      (k,m') = insert e m2
  in  DAG m'
exp_to_dag _ d = d

dag_4 = exp_to_dag exp_mul4 (DAG empty)

dag_8 = exp_to_dag exp_mul8 (DAG empty)

bench_mul n = 
  case exp_to_dag (mul n (Variable "i")) (DAG empty) of DAG d -> size d

bench20 = bench_mul (2^20)
bench21 = bench_mul (2^21)

test_skl_dag = foldl (flip exp_to_dag) (DAG empty) (test_sklansky_i 4)

bench_skl n = case foldl (flip exp_to_dag) (DAG empty) (test_sklansky_i n) of
  DAG d -> size d