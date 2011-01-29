------------------------------------------------------------------------------
--
-- Example with simple main action with simple actions.
--
------------------------------------------------------------------------------

module Main where

import Control.Concurrent
import Control.Monad (forever)

import Sound.SC3

import S3

main :: IO ()
main = serveDefault [("foo",foo),("bar",bar),("buzz",buzz)]

foo = mkAct "foo" 0.5
bar = mkAct "bar" 1.5
buzz = mkAct "buzz" 0.751

mkAct def del var = forever $ do
  -- Without this readMVar, action cannot be paused...
  -- Would it be better to do this within server side code?
  -- Otherwise, forked thread will be unpausable.
  readMVar var
  withSC3 $ \fd -> send fd $ s_new def (-1) AddToTail 1 []
  threadDelay (round $ del * 1e6)
