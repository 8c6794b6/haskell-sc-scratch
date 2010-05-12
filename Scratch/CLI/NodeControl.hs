------------------------------------------------------------------------------
-- | Scratch to play with command line interface to control node's params.
--
-- After adding synth node to scsynth, try giving the node id as argument and
-- start this code. 
-- 

module Scratch.CLI.NodeControl where

import Control.Monad (join)
import Control.Monad.State (lift)
import Data.Generics
import Data.Maybe
import System.Environment (getArgs)
import Text.Parsec

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Wing hiding (string)
import System.Console.Haskeline

main :: IO ()
main = do
  arg <- getArgs
  nodeControl (read $ head arg) s 

-- | Start a command line interface for controlling specifyed node in synth server.
nodeControl :: Transport t => Int -> IO t -> IO ()
nodeControl nid fd = do
  runInputT setting01 loop
      where
        loop :: InputT IO ()
        loop = do
          inputLine <- getInputLine ("nid:" ++ show nid ++ "> ")
          case inputLine of
            Nothing    -> return ()
            Just "q"   -> return ()
            Just "h"   -> lift showHelp >> loop
            Just ""    -> loop
            Just input -> 
               lift (runCmd nid (maybe Ignore id (parseCmd input)) fd) >> 
               loop

-- | Setting for haskeline cli loop.
setting01 :: Settings IO
setting01 = defaultSettings

-- | Localhost, UDP 57110.
defaultScSynth :: IO UDP
defaultScSynth = openUDP "127.0.0.1" 57110

-- | Show help message.
showHelp :: IO ()
showHelp = putStrLn helpMessage

helpMessage :: String
helpMessage = 
    "Commands:\n\
    \ dump           - dump the node's param names and values.\n\
    \ free           - free the node.\n\
    \ get NAME       - get the value of param.\n\
    \ set NAME VALUE - set the value of param.\n\
    \ q              - quit.\n\
    \ h              - show this help."

-- | Commands for node.
data Cmd = Dump
         | Free
         | Get String
         | Set String Double
         | Ignore
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
--   * Free the node.
-- 
parseCmd :: String -> Maybe Cmd
parseCmd xs = case res of 
                Left _ -> Nothing
                Right cmd -> Just cmd
    where
      res = runParser cmdParser "" "" xs 

cmdParser :: Parsec String a Cmd
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
parseSet = do
  (n,v) <- do { string "set";
                spaces;
                name <- many (alphaNum <|> oneOf "._");
                spaces ;
                value <- many (digit <|> oneOf "-.");
                return (name, value)}
  return (Set n (read v))

-- | Runs command with given nodeId and given server.
runCmd :: Transport t => Int -> Cmd -> IO t -> IO ()
runCmd n Dump t = wT t $ \fd -> do
  osc <- queryTree fd
  let node = getNodeById n (parseOSC osc)
      params = fmap getParams node
  maybe (return ()) (mapM_ print) params
runCmd n Free t = wT t (\fd -> send fd $ n_free [n])
runCmd n (Get p) t = wT t $ \fd -> do
  send fd $ s_get n [p]
  Message _ dtms <- wait fd "/n_set"
  let [(Float v)] = drop 2 dtms
  print v
runCmd n (Set p v) t = wT t (\fd -> send fd $ n_set n [(p, v)])
runCmd _ Ignore _ = return ()


-- | Get specified node from tree.
getNodeById :: Data a => Int -> a -> Maybe SCTree
getNodeById nid a = listToMaybe $ everything (++) ([] `mkQ` f) a
    where
      f s@(Synth nid' _ _) | nid == nid' = [s]
                           | otherwise   = []
      f _ = []

-- | Get parameter name and value from Synth. Empty list would be returned when
-- Group is given.
getParams :: SCTree -> [SynthParam]
getParams (Group _ _) = []
getParams (Synth _ _ ps) = ps

-- | Variant of @withTransport@.
wT :: (Transport t) => IO t -> (t -> IO a) -> IO a
wT = withTransport

--
-- For testing
--

-- | Adds default synth.
addTestNode :: Int -> IO ()
addTestNode nid = withSC3 $ \fd -> send fd msg
    where
      msg = s_new "default" nid AddToTail 1 []

