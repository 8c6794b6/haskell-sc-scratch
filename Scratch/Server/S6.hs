{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable (ghc concurrency)

Server to manage patterns.

-}
module S6 where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv,sendAll)

import Data.Serialize
import Data.String
import Sound.SC3
import Sound.SC3.Lepton

import qualified Data.ByteString.Char8 as C8

main :: IO ()
main = server

{-|

Simple server to run patterns.

This server received bytestring representation of pattern expression,
convert it to runnable data type, and delegates messages translated from
pattern to scsynth server.

-}
server :: IO ()
server = withSocketsDo $ bracket acquire release work
  where
    acquire = do
      (serveraddr:_) <- getAddrInfo
        (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "3000")
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      bindSocket sock (addrAddress serveraddr)
      listen sock 1
      return sock
    release sock = do
      putStrLn "Closing socket"
      sClose sock
    work = talk
    talk sock = forever $ bracket
        (do (con,_) <- accept sock
            return con)
        (\con -> do
            putStrLn "Closing connection"
            sClose con)
        (\con -> do
            msg <- recv con 4096
            unless (C8.null msg) $ do
              putStr "Got: "
              C8.putStrLn msg
              let r :: Either String (R (ToOSC Double))
                  r = decode msg >>= fromExpr
              case r of
                Right pat -> withSC3 $ \fd -> play fd pat
                Left err  -> putStrLn err)

{-|

Simple client.

Sends given pattern expression to above server.

-}
client :: Expr (ToOSC Double) -> IO ()
client pattern = withSocketsDo $ bracket acquire release work
  where
    acquire = do
      (serveraddr:_) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3000")
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      connect sock (addrAddress serveraddr)
      return sock
    release sock = sClose sock
    work sock = sendAll sock $ encode $ toExpr pattern
    
-- ---------------------------------------------------------------------------
-- Sample patterns

pspe = psnew "speSynth" Nothing AddToTail 1
  [("dur", prepeat 0.13)
  ,("freq", midiCPS pspeFreq)]

pspeFreq =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

psw = pappend set03 (ppar [loop01, loop02, loop03])

loop01 = psnew "rspdef1" Nothing AddToTail 1
  [("dur", pcycle [preplicate 1024 (1/41)
                  ,preplicate 512 (2/41)
                  ,preplicate 256 (4/41)
                  ,preplicate 128 (8/41)])
  ,("freq", midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  ,("n_map/fmul", pforever 100)]

loop02 = psnew "rspdef2" Nothing AddToTail 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp $ prange (log 110) (log 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 = pnset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

set03 = psnew "rspdef3" (Just 1003) AddToHead 1 [("dur",pval 0.1)]
