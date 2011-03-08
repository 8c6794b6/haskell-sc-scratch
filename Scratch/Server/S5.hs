------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (concurrency specific to ghc)
--
-- Playing with concurrency, server side, take 5.
-- Experiment with updating consumed contents.
--
module S5 where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import qualified Data.Map as M

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton

import Client

-- $idea
--
-- Few ideas I'm thinking now:
-- 
-- * Separate OSC command for managing threads and resources.
-- 
-- * Resume threads after specifyed delay to synchronize.
-- 
-- * Handle delay time differently from other resources.
-- 
-- * Use OSC as pattern value.
-- 
-- * Use SCNode as pattern value, and parse as OSC.
-- 
-- * Use Double as pattern value, and specify s_new or n_set.
-- 

main :: IO ()
main = serve "127.0.0.1" 57130

w = withSC3

-- See "Graceful Exit" in haskell wiki to kill the consumer thread.
serve host port = bracket (udpServer host port) close $ \con -> do
  putStrLn $ "Server started at host:" ++ host ++ ", port: " ++ show port
  sendFoo
  emv <- newEmptyMVar
  tidmv <- newMVar Nothing
  t0 <- forkIO $ forever $ consume emv
  forever $ recv con >>= respond emv tidmv

sendFoo :: IO OSC
sendFoo = withSC3 $ \fd -> 
  async fd $ d_recv $ synthdef "foo" $ 
  out 0 $ sinOsc ar (control kr "freq" 440) 0 * 0.3 *
  xLine kr 1 1e-9 800e-3 RemoveSynth

consume :: (Show a, Integral a) => MVar a -> IO ()
consume mv = do 
  v <- takeMVar mv
  withSC3 $ \fd -> 
    send fd $ s_new "foo" (-1) AddToTail 1 [("freq",midiCPS $ fromIntegral v)]
  threadDelay (5*10^5)
  
respond :: MVar Int -> MVar (Maybe ThreadId) -> OSC -> IO ()  
respond emv tmv msg = do
  case msg of 
    Message "/rep" xs  -> go . cycle $ concatMap unInt xs
    Message "/once" xs -> go $ concatMap unInt xs
    _                  -> putStrLn $ "Malformed message: " ++ show msg
  where 
   go vs = do
      tnew <- forkIO $ writer emv vs
      modifyMVar_ tmv $ \tid -> do
        case tid of
          Nothing   -> return (Just tnew)
          Just told -> killThread told >> return (Just tnew)
    
writer :: MVar a -> [a] -> IO ()
writer mv (x:xs) = putMVar mv x >> writer mv xs
writer _  []     = return ()
      
unInt :: Datum -> [Int]
unInt (Int i) = [i]
unInt _       = []
  
-- testing
  
go1 :: IO ()
go1 = do
  m <- newEmptyMVar
  
  -- Consumer, read from MVar and print its value
  t0 <- forkIO $ forever $ do
        v <- takeMVar m
        print v
        threadDelay $ 5*10^5
        
  -- First writer, repeating 10.
  t1 <- forkIO $ writer m (repeat 10)
  threadDelay $ 3*10^6
  
  -- Second writer, cycling from 1 to 5.
  killThread t1  
  t2 <- forkIO $ writer m (cycle [1..5])
  
  -- Run for 10 seconds and kill threads
  threadDelay $ 10*10^6
  mapM_ killThread [t0,t2]
  where
    writer m (x:xs) = putMVar m x >> writer m xs
    writer _ []     = return ()
