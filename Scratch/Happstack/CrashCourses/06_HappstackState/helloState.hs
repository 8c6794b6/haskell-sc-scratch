{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/HappstackState.html>

-}
module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (msum)
import Data.Data (Data,Typeable)

import Control.Monad.Reader (ask)
import Control.Monad.State (get,put)
import Happstack.Server
  (Response, ServerPart, dir, nullDir, nullConf, ok, simpleHTTP, toResponse)
import Happstack.State
  (Component(..), End, Proxy(..), Query, Update, Version
  ,createCheckpoint, deriveSerialize, mkMethods, query, startSystemState
  ,shutdownSystem, update)

newtype Counter = Counter {unCounter::Integer}
  deriving (Eq, Num, Enum, Ord, Read, Show, Data, Typeable)

instance Version Counter
$(deriveSerialize ''Counter)

data AppState = AppState
  { count :: Counter
  } deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Version AppState
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState { count = 0 }

addCounter :: Integer -> Update AppState Counter
addCounter n = do
  appState <- get
  let newCount = (count appState) + (Counter n)
  put $ appState {count = newCount}
  return newCount

peekCounter :: Query AppState Counter
peekCounter = count <$> ask

$(mkMethods ''AppState ['addCounter, 'peekCounter])

main :: IO ()
main = do
  bracket (startSystemState (Proxy :: Proxy AppState))
    createCheckPointAndShutdown $ \_ ->
      simpleHTTP nullConf handlers
  where
    createCheckPointAndShutdown ctrl = do
      createCheckpoint ctrl
      shutdownSystem ctrl

handlers :: ServerPart Response
handlers =
  msum [ dir "peek" $ do
            c <- query PeekCounter
            ok $ toResponse $
              "peeked at the count and saw: " ++ show (unCounter c) ++ "\n"
       , do nullDir
            c <- update (AddCounter 1)
            ok $ toResponse $ "New count is: " ++ show (unCounter c) ++ "\n"
       ]
