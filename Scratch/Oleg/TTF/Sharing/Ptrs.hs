{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/Ptrs.hs>

-}
module Ptrs where

import Control.Monad.State

import Data.IORef
import System.IO.Unsafe

type Lab = Int

data ExpL
  = AddL Lab ExpL ExpL
  | VariableL Lab String
  | ConstantL Lab Int
  deriving Show

instance Eq ExpL where
  e1 == e2 = label e1 == label e2
    where
      label (AddL p _ _)    = p
      label (VariableL p _) = p
      label (ConstantL p _) = p

------------------------------------------------------------------------------
-- First approach: Manual labeling

expL_a = AddL 3 (ConstantL 1 10) (VariableL 2 "i1")
expL_b = AddL 4 expL_a (VariableL 5 "i2")

-- | The function takes extra argument, initial counter.
-- and returns the finaal counter as the extra result.
--
mulL :: Int -> ExpL -> Lab -> (Lab,ExpL)
mulL 0 _ p = (p+1, ConstantL p 0)
mulL 1 x p = (p,x)
mulL n x p | even n = mulL (n `div` 2) (AddL p x x) (p+1)
           | otherwise = let (p',r) = mulL (n-1) x p in (p'+1, AddL p' x r)

expL_mul4 = mulL 4 (VariableL 0 "i1") 1
expL_mul8 = mulL 8 (VariableL 0 "i1") 1

------------------------------------------------------------------------------
-- Second approach: Using state monad

type ExpM = State Lab ExpL

new_labelM :: State Lab Lab
new_labelM = do
  p <- get
  put (p+1)
  return p

varM :: String -> ExpM
varM v = new_labelM >>= \p -> return $ VariableL p v

constM :: Int -> ExpM
constM x = new_labelM >>= \p -> return $ ConstantL p x

addM :: ExpL -> ExpL -> ExpM
addM x y = new_labelM >>= \p -> return $ AddL p x y

-- NG. ... ?
addM' :: ExpM -> ExpM -> ExpM
addM' x y = do
  xv <- x
  yv <- y
  p <- new_labelM
  return $ AddL p xv yv

run_expM :: ExpM -> ExpL
run_expM e = evalState e 0

expM_a = do
  xv <- constM 10
  yv <- varM "i1"
  addM xv yv

expM_b = do
  xv <- expM_a
  yv <- varM "i2"
  addM xv yv

expM_c = do
  xv <- expM_a
  yv <- expM_a
  addM xv yv

mulM :: Int -> ExpL -> ExpM
mulM 0 _ = constM 0
mulM 1 x = return x
mulM n x
  | even n    = mulM (n `div` 2) =<< addM x x
  | otherwise = addM x =<< mulM (n-1) x

-- NG. Not sharing.
-- To pass ExpL to add, we use addM instead of addM'.
mulM' :: Int -> ExpM -> ExpM
mulM' 0 _ = constM 0
mulM' 1 x = x
mulM' n x
  | even n    = mulM' (n `div` 2) (addM' x x)
  | otherwise = addM' x (mulM' (n-1) x)

expM_mul4 = mulM 4 =<< varM "i1"
expM_mul8 = mulM 8 =<< varM "i1"

------------------------------------------------------------------------------
-- Third approach, Observable sharing
--
-- XXX:
-- NOINLINE pragma not working?
-- When compiled with '-O', IORef example is showing 0 for labels in all
-- expressions.
--

{-# NOINLINE counter #-}
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

new_label :: () -> Int
new_label x = unsafePerformIO $ do
  p <- readIORef counter
  writeIORef counter (p+1)
  return p

varU :: String -> ExpL
varU v = VariableL (new_label ()) v

constU :: Int -> ExpL
constU x = ConstantL (new_label ()) x

addU :: ExpL -> ExpL -> ExpL
addU x y = AddL (new_label ()) x y

expU_a = addU (constU 10) (varU "i1")
expU_b = addU expU_a (varU "i2")

mulU :: Int -> ExpL -> ExpL
mulU 0 _ = constU 0
mulU 1 x = x
mulU n x
  | even n    = mulU (n `div` 2) (addU x x)
  | otherwise = addU x (mulU (n-1) x)

expU_mul4 = mulU 4 (varU "i1")
expU_mul8 = mulU 8 (varU "i1")

main = do
  print expL_mul8
  print $ run_expM expM_mul4
  print $ expU_b
  print expU_mul8