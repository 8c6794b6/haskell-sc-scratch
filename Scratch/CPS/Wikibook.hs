{-# LANGUAGE PackageImports #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From: <http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style>
--
module Wikibook where

import Data.Char (digitToInt, intToDigit)

import "mtl" Control.Monad.Cont

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

add'cps :: Int -> Int -> (Int -> r) -> r
add'cps x y k = k (add x y)

square'cps :: Int -> (Int -> r) -> r
square'cps x k = k (square x)

pythagoras'cps :: Int -> Int -> (Int -> r) -> r
pythagoras'cps x y k = 
  square'cps x $ \x' ->
  square'cps y $ \y' ->
  add'cps x' y' $ \r ->
  k r
  
thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

thrice'cps :: (a -> (a -> b) -> b) -> a -> (a -> b) -> b
thrice'cps f x k =
  f x $ \x' ->
  f x' $ \x'' ->
  f x'' $ \x''' ->
  k x'''

add'm :: Monad m => Int -> Int -> m Int
add'm x y = return (add x y)

square'm :: Monad m => Int -> m Int
square'm x = return (square x)

pythagoras'm :: Monad m => Int -> Int -> m Int
pythagoras'm x y = do
  x' <- square'm x
  y' <- square'm y
  add'm x' y'
  
squareCC :: (MonadCont m, Num b) => b -> m b
squareCC n = callCC $ \k -> k (n^2)  

foo :: (Num a, MonadCont m, Ord a) => a -> m String
foo n = callCC $ \k -> do
  let n' = n^2+3
  when (n'>20) $ k "over twenty"
  return $ show $ n'-4

bar :: (MonadCont m) => Char -> String -> m (Int, String)
bar c s = do
  msg <- callCC $ \k -> do
    let s' = c:s
    when (s' == "hello") $ k "They say hello"
    let s'' = show s'
    return $ "They appear to be saying " ++ s''
  return (length msg, msg)  
  
fun :: MonadCont m => Int -> m String
fun n = do
  str <- callCC $ \exit1 -> do
    when (n<10) (exit1 $ show n)
    let ns = map digitToInt (show $ n `div` 2)
    n' <- callCC $ \exit2 -> do
      when (length ns < 3) $ exit2 $ length ns
      when (length ns < 5) $ exit2 n
      when (length ns < 7) $ do
        let ns' = map intToDigit (reverse ns)
        exit1 $ dropWhile (=='0') ns'
      return $ sum ns
    return $ "(ns = " ++ show ns ++ ") " ++ show n'
  return $ "Answer: " ++ str