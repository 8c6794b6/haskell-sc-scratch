------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Dying philosopher problem example with STM, from:
--
-- * <http://computationalthoughts.blogspot.com/2008/03/some-examples-of-software-transactional.html>
--
module DP where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

type Semaphore = TVar Bool

newSem :: Bool -> IO Semaphore
newSem = newTVarIO

p :: Semaphore -> STM ()
p sem = do
  b <- readTVar sem
  if b
    then writeTVar sem False
    else retry

v :: Semaphore -> STM ()
v sem = writeTVar sem True

type Buffer a = TVar [a]

newBuffer :: IO (Buffer a)
newBuffer = newTVarIO []

put :: Buffer a -> a -> STM ()
put buffer item = do
  ls <- readTVar buffer
  writeTVar buffer (ls++[item])

get :: Buffer a -> STM a
get buffer = do
  ls <- readTVar buffer
  case ls of
    (item:rest) -> writeTVar buffer rest >> return item
    _           -> retry

type Resource = TVar Int

acquire :: Resource -> Int -> STM ()
acquire res nr = do
  n <- readTVar res
  if n >= nr
     then writeTVar res (n-nr)
     else retry

release :: Resource -> Int -> STM ()
release res nr = do
  n <- readTVar res
  writeTVar res (n+nr)

simulation :: Int -> IO a
simulation n = do
  forks <- replicateM n (newSem True)
  outputBuffer <- newBuffer
  forM_ [0..n-1] $ \i ->
    forkIO (philosopher i outputBuffer
            (forks!!i) (forks!!((i+1) `mod` n)))
  output outputBuffer

output :: Buffer String -> IO a
output buffer = do
  str <- atomically $ get buffer
  putStrLn str
  output buffer

philosopher :: Int -> Buffer String -> Semaphore -> Semaphore -> IO ()
philosopher n out fork1 fork2 = do
  atomically $ put out $ "Philosopher " ++ show n ++ " is thinking."

  randomDelay

  atomically $ do
    p fork1
    p fork2

  atomically $ put out $ "Philosopher " ++ show n ++ " is eating."
  randomDelay

  atomically $ do
    v fork1
    v fork2

  philosopher n out fork1 fork2

randomDelay :: IO ()
randomDelay = do
  d <- randomRIO (1e6,5e6::Double)
  threadDelay $ floor d