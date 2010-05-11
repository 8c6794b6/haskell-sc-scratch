------------------------------------------------------------------------------
-- | Scratch to play with command line interface to control node's params.
--

module Scratch.CLI.NodeControl where

import Control.Applicative
import Control.Monad.State
import System.Environment (getArgs)
import Text.Parsec

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing
import System.Console.Haskeline

main :: IO ()
main = undefined

-- | Start a command line interface for controlling specifyed node in synth server.
nodeControl :: Transport t => Int -> t -> IO ()
nodeControl nodeId fd = do
  runInputT setting01 loop
      where
        loop :: InputT IO ()
        loop = do
          inputLine <- getInputLine ("node:" ++ show nodeId ++ "> ")
          case inputLine of
            Nothing    -> return ()
            Just "q"   -> return ()
            Just "h"   -> lift showHelp >> loop
            Just ""    -> loop
            Just input -> lift (runCmd nodeId (parseCmd input) fd) >> loop

-- | Setting for haskeline cli loop.
setting01 :: Settings IO
setting01 = defaultSettings

-- | Localhost, UDP 57110.
defaultScSynth :: IO UDP
defaultScSynth = openUDP "127.0.0.1" 57110

-- | Show help message.
showHelp :: IO ()
showHelp = putStrLn "help is not available yet. \
                     \Wait a bit."

-- | Commands for node.
data Cmd = Dump
         | Free
         | Get String
         | Set String Double
           deriving (Eq, Show, Read)

-- | Parses input.
parseCmd :: String -> Cmd
parseCmd xs = undefined

-- | Runs command with given nodeId and given server.
runCmd :: Transport t => Int -> Cmd -> t -> IO ()
runCmd n Dump t = undefined
runCmd n Free t = wT t (\fd -> send fd $ n_free [n])
runCmd n (Get p) t = wT t $ \fd -> do
  send fd $ s_get n [p]
  Message _ dtms <- wait fd "/n_set"
  let [(Float v)] = drop 2 dtms
  print v
runCmd n (Set p v) t = wT t (\fd -> send fd $ n_set n [(p, v)])

-- | Variant of @withTransport@.
wT :: (Transport t) => t -> (t -> IO a) -> IO a
wT t = withTransport (return t)

-- | Adds default synth.
addTestNode :: Int -> IO ()
addTestNode nid = withSC3 $ \fd -> send fd msg
    where
      msg = s_new "default" nid AddToTail 1 []

tryNQuery :: Int -> IO OSC
tryNQuery nid =
    withSC3 $ \fd -> do
      send fd $ notify True
      wait fd "/done"
      send fd $ n_query [nid]
      wait fd "/n_info"
