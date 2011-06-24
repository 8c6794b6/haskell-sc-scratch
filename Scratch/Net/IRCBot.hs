------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- From:
--
-- * http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot
--
module IRCBot where

import Network
import System.IO
import Text.Printf

server = "irc.freenode.org"
port = 6667
chan = "#tutbot-testing"
nick = "tutbot"

main :: IO ()
main = do
  h <- connectTo server (PortNumber port)
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick ++ " 0 * :test bot")
  write h "JOIN" chan
  listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
  s <- hGetLine h
  putStrLn s
  where
    forever a = a >> forever a
