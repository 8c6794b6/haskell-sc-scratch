{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Comparing Riemman's zeta function computed with MVar, Strategies, and Chan.

-}
module Zeta where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Complex
import System.Environment (getArgs)

import Criterion.Main

main :: IO ()
main = do
  params <- getParams
  -- print =<< goMVar params
  print =<< goStrategies params
  -- print =<< goChan params
  -- print =<< goTMVar params
  -- defaultMain
  --   [ bench "mvar" (whnfIO (goMVar params))
  --   , bench "strategies" (whnfIO (goStrategies params))
  --   , bench "chan" (whnfIO (goChan params)) ]

zetaRange :: (Floating a, Integral b) => a -> (b, b) -> [a]
zetaRange s (x, y) = [fromIntegral n ** (-s) | n <- [x..y]]

cut :: (Integral a) => (a, a) -> a -> [(a, a)]
cut (x,y) n = (x, x+mine-1) : cut' (x+mine) size (y-mine) where
  (size, modulo) = y `divMod` n
  mine = size + modulo
  cut' _  _     0  = []
  cut' x' size' n' = (x',x'+size'-1) : cut' (x'+size') size' (n'-size')

getParams :: IO (Int, Int, Complex Double)
getParams = do
  argv <- getArgs
  case argv of
    (t:n:s:[]) -> return (read t, read n, read s)
    _          -> error "usage: Zeta <nthreads> <boundary> <s>"

goMVar (t,n,s) = do
  childs <- mapM (thread s) (cut (1, n) t)
  results <- mapM takeMVar childs
  return $ sum $ concat results
  where
    thread s range = do
      mvar <- newEmptyMVar
      forkIO $ do
        let zs = zetaRange s range
        zs `deepseq` putMVar mvar zs
      return mvar

goStrategies (t,n,s) = do
  let ranges = cut (1, n) t
      results = map (zetaRange s) ranges `using` parList rdeepseq
  return $ sum $ concat results

-- goChan (t,n,s) = do
--   chan <- newChan
--   terms <- getChanContents chan
--   forM_ (cut (1,n) t) $ thread chan s
--   let wait xs i result
--         | i >= t    = return result
--         | otherwise = case xs of
--           Nothing : rest -> wait rest (i+1) result
--           Just x  : rest -> wait rest i (result+x)
--           _              -> error "Missing thread termination marker"
--   wait terms 0 0
--   where
--     thread chan s range = do
--       forkIO $ do
--         mapM_ (writeChan chan . Just) (zetaRange s range)
--         writeChan chan Nothing

goTMVar (t,n,s) = do
  childs <- mapM (thread s) (cut (1, n) t)
  results <- atomically $ mapM (takeTMVar) childs
  return $ sum $ concat results
  where
    thread s range = do
      tmvar <- newEmptyTMVarIO
      forkIO $ do
        let zs = zetaRange s range
        zs `deepseq` (atomically $ putTMVar tmvar zs)
      return tmvar
