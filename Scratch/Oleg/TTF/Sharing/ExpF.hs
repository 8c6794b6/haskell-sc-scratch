{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/ExpF.hs>

-}
module ExpF
  ( Exp(..)
  , ExpI(..)
  , R(..)
  , N(..)
  , NodeId
  , DAG(..)
  , run_expN
  , do_bench
  , mul
  , main
  ) where

import Control.Monad.State

import BiMap
import qualified ExpI (Exp(..), sklansky)

class Exp repr where
  constant :: Int -> repr Int
  variable :: String -> repr Int
  add      :: repr Int -> repr Int -> repr Int

exp_a = add (constant 10) (variable "i1")
exp_b = add exp_a (variable "i2")

mul :: Exp repr => Int -> repr Int -> repr Int
mul 0 _ = constant 0
mul 1 x = x
mul n x
  | even n    = mul (n `div` 2) (add x x)
  | otherwise = add x (mul (n-1) x)

exp_mul4 = mul 4 (variable "i1")
exp_mul8 = mul 8 (variable "i1")

newtype ExpI t = ExpI (ExpI.Exp)

instance Exp ExpI where
  constant = ExpI . ExpI.Constant
  variable = ExpI . ExpI.Variable
  add (ExpI x) (ExpI y) = ExpI (ExpI.Add x y)

test_shb = case exp_b of ExpI e -> e

test_sh4 = case exp_mul4 of ExpI e -> e

type REnv = [(String,Int)]
newtype R t = R {unR :: REnv -> t}

instance Exp R where
  constant x = R $ \_ -> x
  variable x = R $ \env -> maybe (error $ "no var: " ++ x) id $ lookup x env
  add e1 e2 = R $ \env -> unR e1 env + unR e2 env

test_val4 = unR exp_mul4 [("i1",5)]

type NodeId = Int

data Node
  = NConst Int
  | NVar String
  | NAdd !NodeId !NodeId
    deriving (Eq,Ord,Show)

newtype DAG = DAG (BiMap Node) deriving Show

hashcons :: Node -> State DAG NodeId
hashcons e = do
  DAG m <- get
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in  put (DAG m') >> return k
    Just k  -> return k

newtype N t = N {unN :: State DAG NodeId}

instance Exp N where
  constant x = N (hashcons $ NConst x)
  variable x = N (hashcons $ NVar x)
  add e1 e2 = N $ do
    h1 <- unN e1
    h2 <- unN e2
    hashcons $ NAdd h1 h2

run_expN :: N t -> (NodeId, DAG)
run_expN (N m) = runState m (DAG empty)

test_sklansky_o n = ExpI.sklansky addition xs where
  addition x y = "(" ++ x ++ "+" ++ y ++ ")"
  xs = Prelude.map (("v"++) . show) [1..n]

test_sklansky n = runState sk (DAG empty) where
  sk = sequence (map unN (ExpI.sklansky add xs))
  xs = map (variable . show) [1..n]

do_bench :: (a,DAG) -> Int
do_bench (_,DAG d) = size d

bench_mul n = do_bench $ run_expN (mul n (variable "i"))

bench_skl n = do_bench $ test_sklansky n

main = do
  -- print $ bench_mul (2^23) -- maxBound
  print $ bench_skl 1024
