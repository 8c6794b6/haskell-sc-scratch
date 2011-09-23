{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (Rank2Types, FlexibleContexts)

DSL Sharing tutorial from:

* <http://okmij.org/ftp/tagless-final/sharing/ExpLet.hs>

-}
module ExpLet where

import ExpF hiding (main)

exp_mul4 = 
  let x = variable "i1" in
  let y = add x x in
  add y y
  
dag_mul4 = run_expN exp_mul4  

newtype Size t = Size {unSize :: Int}

instance Exp Size where
  constant _ = Size 1
  variable _ = Size 1
  add (Size x) (Size y) = Size (x+y+1)
  
size_mul4 = unSize exp_mul4   
size_large = unSize $ mul (2^30) (variable "i1")

newtype Print t = Print {unPrint :: IO ()}

instance Exp Print where
  constant = Print . putStr . show
  variable = Print . putStr
  add (Print x) (Print y) = Print (x >> putStr " + " >> y)
  
print_mul4 = unPrint $ exp_mul4

pm30 = unPrint $ mul (2^30) (variable "i1")

class ExpLet repr where
  let_ :: repr a -> (repr a -> repr b) -> repr b
  
exp_mul4' =   
  let_ (variable "i1") $ \x ->
  let_ (add x x) $ \y ->
  add y y

instance ExpLet R where
  let_ x f = f x

val_mul4 = unR exp_mul4 [("i1",5)]
val_mul4' = unR exp_mul4' [("i1",5)]

type LetVarCount = Int
newtype S t = S {unS :: LetVarCount -> String}

instance Exp S where
  constant = S . const . show
  variable = S . const
  add e1 e2 = S $ \c -> unS e1 c ++ " + " ++ unS e2 c
  
instance ExpLet S where  
  let_ e f = S $ \c ->
    let vname = "v" ++ show c
    in  unwords ["let",vname,"=",unS e c,"in"
                ,unS (f (S (const vname))) (succ c)]
        
run_expS :: S t -> String
run_expS (S m) = m 0

sh_mul4 = run_expS exp_mul4
sh_mul4' = run_expS exp_mul4'

instance ExpLet N where
  let_ e f = N $ do
    x <- unN e
    unN $ f (N (return x))
    
dag_mul4' = run_expN exp_mul4'    

mul' :: (ExpLet repr, Exp repr) => Int -> repr Int -> repr Int
mul' 0 _ = constant 0
mul' 1 x = x
mul' n x 
  | even n    = let_ x $ \x' -> mul' (n `div` 2) (add x' x')
  | otherwise = add x (mul' (n-1) x)
  
sh_mul4'' = run_expS (mul' 4 (variable "i1"))

bench_mul' n = do_bench $ run_expN (mul' n (variable "i"))

main = do
  print $ bench_mul' maxBound -- (2^03)
