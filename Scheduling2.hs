------------------------------------------------------------------------------
-- | Server client scratch, take 2.
--

module Scheduling2 where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket
import Network.BSD
import qualified Data.Map as M

import Sound.OpenSoundControl 
    ( OSC(..),
      Datum(..))
import qualified Sound.OpenSoundControl as OSC
import Sound.SC3 hiding (send)

import SCTree
import SCQuery
import Reusable


-- | Datatype for sequencial actions.
data Action a b = Action (a -> b -> IO a) a [b]

action :: Action a b -> IO a
action (Action f v xs) = foldM f v xs

act01 :: Action () Double
act01 = Action f () [440,880,660,330,220]
    where
      f a b = do
        withSC3 (\fd -> OSC.send fd $ s_new "default" (-1) AddToTail 1 [("freq",b)])
        threadDelay (round 1e6)

-- | Send some OSC messages to specified port in localhost.
sendSomething :: Int -> OSC -> IO ()
sendSomething port msg
    = OSC.withTransport (OSC.openUDP "127.0.0.1" port) (\udp -> OSC.send udp msg)

-- | Listens to specified port in localhost.
-- Try sending from ghci with:
-- > > withTransport (openUDP "127.0.0.1" 58220) (\u -> send u (Message "/foo" []))
simpleServer :: Int -> OSCHandler -> IO ()
simpleServer port handler = OSC.withTransport (OSC.udpServer "127.0.0.1" port) g
    where
      g udp = forever $ do
                msg <- OSC.recv udp
                putStr $ show port ++ ": "
                handler msg

doSomething :: OSC -> IO ()
doSomething msg = withSC3 (\fd -> OSC.send fd msg) >> print msg

type OSCHandler = OSC -> IO ()

type HandlerFunc = SockAddr -> String -> IO ()

secondServer :: Int -> HandlerFunc -> IO a
secondServer port handlerfunc = withSocketsDo $
  do addrinfos <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing (Just $ show port)
     let serveraddr = head addrinfos
     sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
     bindSocket sock (addrAddress serveraddr)
     procMessages sock
    where
      procMessages sock =
          do (msg,_,addr) <- recvFrom sock 4096
             handlerfunc addr msg 
             procMessages sock

plainHandler :: HandlerFunc
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg
    

