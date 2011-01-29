------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with concurrency, take 2.
--
module S2 where

import Control.Monad
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as M

import qualified Network.Socket as N

main :: IO ()
main = serveThreads "127.0.0.1" 57130 [("foo", foo),("bar",bar)]

foo = forever $ do
  putStrLn "foo"
  threadDelay (floor $ 1.321 * 1e6)

bar = forever $ do
  putStrLn "bar"
  threadDelay (floor $ 0.987 * 1e6)

--
-- Server side
--

-- | Serve threads. Threads could be referred later with its name.
serveThreads :: String           -- ^ Host name
             -> Int              -- ^ Port number
             -> [(String,IO ())] -- ^ List of pair of (thread name, action to fork)
             -> IO ()
serveThreads host port ts = do
  addrinfos <- N.getAddrInfo
    (Just (N.defaultHints { N.addrFlags = [N.AI_PASSIVE]}))
    Nothing (Just $ show port)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
  N.bindSocket sock (N.addrAddress serveraddr)
  e <- initialEnv
  initialAddition e ts
  forever $ procMessages e sock
  where
    procMessages e sock = do
      msg <- N.recv sock 1024
      work msg e

type ServerEnv = MVar (Map String ThreadInfo)

data ThreadInfo = ThreadInfo
  { tiId :: ThreadId
  , tiStatus :: ChildStatus
  , tiBlock :: MVar () }

data ChildStatus = Running
                 | Paused
                   deriving (Eq, Show)

initialEnv :: IO ServerEnv
initialEnv = newMVar M.empty

initialAddition :: ServerEnv -> [(String, IO ())] -> IO ()
initialAddition evar ts = do
  env <- readMVar evar
  tis <- mapM (uncurry forkChild) ts
  modifyMVar_ evar (\m -> return $ foldr (uncurry M.insert) m tis)

forkChild :: String -> IO () -> IO (String, ThreadInfo)
forkChild name act = do
  tid <- forkIO act
  blk <- newMVar ()
  return $ (name, ThreadInfo tid Running blk)

work :: String -> ServerEnv -> IO ()
work msg e = case words msg of
  ["pause",name]  -> putStrLn "pause not implemeted yet"
  ["resume",name] -> putStrLn "resume not implemented yet"
  ["kill",name]   -> killChild e name
  ["add",name]    -> putStrLn "add not implemented yet"
  ["dump"]        -> dumpEnv e
  ["show",name]   -> putStrLn "show not implemented yet"
  _               -> putStrLn $ "Unknown message: " ++ msg


killChild :: ServerEnv -> String -> IO ()
killChild evar name = do
  e <- readMVar evar
  case M.lookup name e of
    Just (ThreadInfo tid _ _) ->
      killThread tid >> modifyMVar_ evar (return . M.delete name)
    Nothing -> putStr $ "No such thead to kill: " ++ name

dumpEnv :: ServerEnv -> IO ()
dumpEnv evar = do
  e <- readMVar evar
  forM_ (M.assocs e) $ \(n,ti) -> do
    putStrLn $ "name: " ++ n
    putStrLn $ show (tiId ti) ++ ", Status: " ++ show (tiStatus ti)

--
-- Client side
--

-- | Dump
dump :: String -> Int -> IO ()
dump host port = client host port "dump"

showThread :: String -> Int -> String -> IO ()
showThread host port name = client host port $ "show " ++ name

-- | Pause thread.
pause :: String -> Int -> String -> IO ()
pause host port name = client host port $ "pause " ++ name

-- | Resume thread.
resume :: String -> Int -> String -> IO ()
resume host port name = client host port $ "resume " ++ name

-- | Kill thread.
kill :: String -> Int -> String -> IO ()
kill host port name = client host port $ "kill " ++ name

-- | Add thread. Action is restricted to Act type, due to its network transportation.
addThread :: String -> Int -> String -> Act -> IO ()
addThread host port name = undefined

connect :: String -> Int -> IO Connection
connect host port = undefined

client :: String  -- ^ Host
       -> Int     -- ^ Port
       -> String  -- ^ Message
       -> IO ()
client host port msg = N.withSocketsDo $ do
  addrinfos <- N.getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
  c <- N.sendTo sock msg (N.addrAddress serveraddr)
  N.sClose sock

data Connection = Connection

data Act = Act
