{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (GHC concurrency)

Server to manage patterns, take 2.

Using STM.

-}
module Sound.SC3.Lepton.Pattern.Server2 where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad

import Control.Monad.Reader
import Sound.OpenSoundControl

import qualified Data.Map as M

{-

------------------------------------------------------------------------------
--
-- Audio Sequence Server spec
--
------------------------------------------------------------------------------

The server receives OSC message, and do tasks according to message.

Message `l_new` will passed with timestamp, thread name and pattern
bytestring.  When server receive this message, plays new pattern with
given name from given timestamp.

Message `l_free` will passed with timestamp, thread name. When server receive
this message, kill the thread with given name at given timestamp.

Message `l_update` will passed with timestamp, thread name and pattern
bytestring.  When server receive this message, stop the thread with
given name and start playing a new thread with given name and new
pattern, from given timestamp.

Message `l_dump` will passed without argument. It will show current
information in server.

Message `l_query` will return the server information to caller.

------------------------------------------------------------------------------
--
-- Log of thoughts, idea, design, etc.
--
------------------------------------------------------------------------------

To respond to given message, we can write a pattern match like:

> respondMessage =
>   case message of
>     LNew time name pattern    -> runNewThread time name pattern
>     LFree time name           -> freeThread time name
>     LUpdate time name pattern -> updateThread time name pattern
>     LDump                     -> dumpServer

Not bad. What's inside runNewThread, freeThread, etc?

It needs a mapping from thread name and thread id, so that we can kill the
thread with specified name.

> data Env1 = Env1 M.Map String ThreadState

Is this enough? We'll see it later, but not a bad start. We want to
make a loop for receiving message and executing
message. OpenSoundControl module has `recv` action to receive message
from specified host and port.

| recv :: Transport t => t -> IO OSC

And there's a trivial udp server that open connection to specified
host and port, bind socket, and returns the connection:

| udpServer :: String -> Int -> IO UDP

Using this, we can write a simple loop like:

> loop = do
>   con <- udpServer "127.0.0.1" 57220
>   forever $ do
>     msg <- recv con
>     respondMessage msg

But we need to pass server env to respondMessage shown above. 
How to do this?

Found an interesting post about using CPS monad, instead of making
stack with mtl.

* http://www.haskell.org/haskellwiki/Performance/Monads
* http://r6.ca/blog/20071028T162529Z.html
* http://www.haskell.org/haskellwiki/MonadCont_under_the_hood

-}

-- | Newtype wrapper for @TVar@ holding @Map@ of @String@ thread names to 
-- @ThreadState@.
data ServerEnv = ServerEnv (TVar (M.Map String ThreadState))

-- newtype Server a = Server {unServer :: ReaderT ServerEnv IO a}
-- newtype Server a = Server {unServer :: forall r. (a -> IO r) -> IO r}

-- CPS style Reader monad with and IO return value
newtype Server a = 
  Server {unServer :: forall r. (a -> (TVar ServerEnv -> IO r)) -> TVar ServerEnv -> IO r}
  
instance Functor Server where  
  fmap f (Server s) = Server $ \k -> undefined
  
instance Applicative Server where  
  pure = return
  (<*>) = ap
  
instance Monad Server where
  return x = Server $ \k -> k x
  Server m >>= f = Server $ \k -> m (\h -> unServer (f h) k)

data ThreadState = Running ThreadId | Done

-- runServer :: forall a. Server ServerEnv -> IO a
runServer (Server s)  = do
  e <- mkInitEnv
  e' <- atomically $ newTVar e
  s return e'
  
mkInitEnv :: IO ServerEnv
mkInitEnv = (atomically $ ServerEnv `fmap` newTVar M.empty)

newtype Svr r a = 
  Svr {unSvr :: (a -> TVar ServerEnv -> IO r) -> TVar ServerEnv -> IO r}
  
instance Monad (Svr r) where
  return x = Svr (\k tv -> k x tv)
  s >>= f  = Svr (\k tv -> unSvr s (\a -> unSvr (f a) k) tv)

-- instance MonadReader r (Svr r) where
--   ask = Svr (\k tv -> undefined)
--   local = undefined