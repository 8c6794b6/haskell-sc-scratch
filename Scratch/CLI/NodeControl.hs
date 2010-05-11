------------------------------------------------------------------------------
-- | Scratch to play with command line interface to control node's params.
--

module Scratch.CLI.NodeControl where

import Control.Applicative hiding ((<|>), many)
import Control.Monad.State
import Data.Generics
import System.Environment (getArgs)
import Text.Parsec

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing hiding (string)
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
            Just input -> 
               lift (runCmd nodeId (maybe Free id (parseCmd input)) fd) >> 
               loop

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
-- 
-- Example inputs:
-- 
-- * set /NAME/ /VALUE/
--   * NAME is name of parameter, VALUE is in Double.
-- * get /NAME/
--   * NAME is name of parameter.
-- * dump 
--   * Dumps the parameter name and value.
-- * free
--   * Frees the node.
-- 
parseCmd :: String -> Maybe Cmd
parseCmd xs = case res of 
                Left _ -> Nothing
                Right cmd -> Just cmd
    where
      res = runParser cmdParser "" "" xs 

cmdParser = parseDump <|> parseFree <|> parseGet <|> parseSet

parseDump :: Parsec String a Cmd
parseDump = string "dump" >> return Dump

parseFree :: Parsec String a Cmd
parseFree = string "free" >> return Free

parseGet :: Parsec String a Cmd
parseGet = do
  name <- do {string "get"; 
              spaces; 
              many (alphaNum <|> oneOf "._")}
  return (Get name)
               
parseSet :: Parsec String a Cmd
parseSet = undefined

-- | Runs command with given nodeId and given server.
runCmd :: Transport t => Int -> Cmd -> t -> IO ()
runCmd n Dump t = wT t $ \fd -> do
  osc <- queryTree fd
  let tree = parseOSC osc
      node = getNodeById n tree
      params = concatMap getParams node
  mapM_ print params
runCmd n Free t = wT t (\fd -> send fd $ n_free [n])
runCmd n (Get p) t = wT t $ \fd -> do
  send fd $ s_get n [p]
  Message _ dtms <- wait fd "/n_set"
  let [(Float v)] = drop 2 dtms
  print v
runCmd n (Set p v) t = wT t (\fd -> send fd $ n_set n [(p, v)])

-- | Get specified node from tree.
getNodeById :: Data a => Int -> a -> [SCTree]
getNodeById nid a = everything (++) ([] `mkQ` f) a
    where
      f s@(Synth _ nid _) = [s]
      f _ = []

-- | Get parameter name and value from Synth. Empty list would be returned when
-- Group is given.
getParams :: SCTree -> [SynthParam]
getParams (Group _ _) = []
getParams (Synth _ _ ps) = ps

-- | Variant of @withTransport@.
wT :: (Transport t) => t -> (t -> IO a) -> IO a
wT t = withTransport (return t)


--
-- For testing
--

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
