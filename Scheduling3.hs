------------------------------------------------------------------------------
-- | Playing with haskell thread.
--

module Scheduling3 where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Map
import Data.Time
import Prelude hiding (catch)
import System.IO.Unsafe

import Sound.OpenSoundControl
import Sound.SC3

type OSCHandler = UDP -> OSC -> IO ()

sendToThird :: Int -> OSC -> IO ()
sendToThird port msg = withTransport (openUDP "127.0.0.1" port) $ 
                       \udp -> send udp msg

thirdServer :: Int -> OSCHandler -> IO a
thirdServer port handler = withTransport (udpServer "127.0.0.1" port) g
    where
      g udp = forever $ do
                putStr $ show port ++ ": " 
                msg <- recv udp
                handler udp msg

handler01 :: UDP -> OSC -> IO ()
handler01 _ msg = print msg >> withSC3 (\fd -> send fd msg) 

handler02 udp (Message msg ps) = 
    case msg of
      "/hello" -> putStrLn "You send me hello"
      "/show" -> putStrLn $ show ps
      "/quit" -> close udp
      "/delay" -> goWithDelay ps 
      _ -> putStrLn $ "I don't understand " ++ msg
handler02 udp (Bundle time ms) = undefined

goWithDelay ((Int n):_) = do
  threadDelay $ n * 10 ^ 6
  putStrLn $ "delayed " ++ show n ++ "s."
goWithDelay _ = error "Delay should come with Int!"

sendToFourth :: (a -> IO ()) -> IO ()
sendToFourth f = undefined

fourthServer = undefined

--
-- Scratch from Control.Concurrent's library reference.
-- 

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())

schedTest01 :: IO ()
schedTest01 = forkIO (write 'a') >> write 'b' 
    where
      write c = putChar c >> threadDelay (round $ 1e5) >> write c