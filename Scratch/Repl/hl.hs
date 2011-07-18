{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Module to play with repl with haskeline. 
--
-- Try main, go, and goGuts to run interactive repls.
--
module Main where

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

import Control.Monad.State
import Control.Monad.Reader
import System.Console.Haskeline
import System.Console.Haskeline.Completion

-- | Simplest example copied from haskeline haddoc.
main :: IO ()
main = putStrLn "InputT Repl example" >> runInputT defaultSettings loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "% "
    case trim <$> minput of
      Nothing     -> loop
      Just "quit" -> outputStrLn "Bye"
      Just ""     -> loop
      Just input  -> outputStrLn ("Input was: " ++ input) >> loop
      
-- | Environment for repl.
data Env = Env 
  { envPrompt :: String 
  , envCount :: Int }
  deriving (Eq, Show)
           
-- | Removes space characters in beginning and end of given String.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace           

go :: IO ()
go = evalStateT (runInputT s loop) 0 where
  s = Settings f Nothing True
    
loop :: (MonadException m, MonadState s m, Show s, Enum s) => InputT m ()
loop = do
  minput <- getInputLine "> "
  case trim <$> minput of
    Nothing -> loop
    Just "show" -> lift get >>= outputStrLn . show >> loop
    Just "succ" -> lift (modify succ) >> loop
    Just "pred" -> lift (modify pred) >> loop
    Just "quit" -> outputStrLn "Bye"
    Just i  | "echo " `isPrefixOf` i -> liftIO (putStrLn (drop 5 i)) >> loop
            | otherwise -> loop

f :: Monad m => CompletionFunc m
f = completeWordWithPrev Nothing " \t" $ \line current -> do
  return $ filter (\x -> matchingPrefix current (display x)) comps 
    where
      comps = map simpleCompletion toplevels
      toplevels = ["quit", "echo", "show", "succ", "pred"]
      matchingPrefix [] [] = True
      matchingPrefix [] ys = True
      matchingPrefix (x:xs) (y:ys) | x == y = matchingPrefix xs ys
                                   | otherwise = False

--
-- Variant of repl using type synonym      
--       

type Repl2 a = InputT (StateT Env IO) a
type Repl2Settings = Settings (StateT Env IO)

go2 :: IO ()    
go2 = evalStateT (runInputT settings2 loop2) (Env "go2> " 0)

loop2 :: Repl2 ()
loop2 = do
  env <- getRepl2
  line <- getInputLine (envPrompt env)
  case trim <$> line of 
    Nothing -> loop2
    Just i 
      | "echo " `isPrefixOf` i -> outputStrLn (drop 5 i) >> loop2
      | i == "quit" -> return ()
      | i == "succ" -> modifyRepl2 (\e -> e {envCount=succ $ envCount e}) >> loop2
      | i == "pred" -> modifyRepl2 (\e -> e {envCount=pred $ envCount e}) >> loop2
      | i == "show" -> getRepl2 >>= outputStrLn . show >> loop2
      | "prompt " `isPrefixOf` i -> 
        modifyRepl2 (setPrompt $ drop 7 i ++ " ") >> loop2
      | otherwise -> loop2

f2 :: Monad m => CompletionFunc m
f2 = completeWordWithPrev Nothing " \t" $ \line current -> do
  return $ filter (\x -> prefixMatch current (display x)) comps where
    comps = map simpleCompletion toplevels
    toplevels = ["quit", "echo", "prompt", "succ", "pred", "show"]
    prefixMatch [] _ = True
    prefixMatch _ [] = False
    prefixMatch (x:xs) (y:ys) | x == y = prefixMatch xs ys
                              | otherwise = False

getRepl2 :: Repl2 Env
getRepl2 = lift get

putRepl2 :: Env -> Repl2 ()
putRepl2 = lift . put

modifyRepl2 :: (Env -> Env) -> Repl2 ()
modifyRepl2 = lift . modify

setPrompt :: String -> Env -> Env
setPrompt s e = e {envPrompt = s}

runRepl2 :: Repl2 a -> Repl2Settings -> Env -> IO a
runRepl2 repl settings env = evalStateT (runInputT settings repl) env

settings2 :: Repl2Settings
settings2 = Settings f2 Nothing True

--
-- Variant using newtype and GeneralizedNewtypeDeriving for inner Monad.
--

-- | Inner guts of repl.
newtype Guts s a = Guts {
  unGuts :: StateT s IO a
  } deriving (Applicative, Functor, Monad, MonadException, MonadIO)
             
instance MonadState s (Guts s) where
  put = Guts . put
  get = Guts get
  
putGuts :: MonadTrans t => s -> t (Guts s) ()
putGuts = lift . put  

getGuts :: MonadTrans t => t (Guts s) s
getGuts = lift get

modifyGuts :: MonadTrans t => (s -> s) -> t (Guts s) ()
modifyGuts f = lift (get >>= put . f)

goGuts :: IO ()
goGuts = runGuts loopGuts (Settings f2 Nothing True) (Env "guts> " 0)

runGuts :: InputT (Guts s) a -> Settings (Guts s) -> s -> IO a
runGuts guts settings env = evalStateT (unGuts (runInputT settings guts)) env

loopGuts :: InputT (Guts Env) ()
loopGuts = do
  minput <- getInputLine . envPrompt =<< getGuts 
  case trim <$> minput of
    Nothing -> loopGuts
    Just i
      | i == ""                  -> loopGuts
      | i == "quit"              -> outputStrLn "guts!"
      | i == "show"              -> getGuts >>= outputStrLn . show >> loopGuts
      | i == "succ"              -> 
        modifyGuts (\e -> e {envCount=succ $ envCount e}) >> loopGuts
      | i == "pred"              -> 
        modifyGuts (\e -> e {envCount=pred $ envCount e}) >> loopGuts
      | "echo " `isPrefixOf` i   -> outputStrLn (drop 5 i) >> loopGuts  
      | "prompt " `isPrefixOf` i -> 
        modifyGuts (setPrompt (drop 7 i ++ " ")) >> loopGuts
      | otherwise                -> 
        outputStrLn ("Unknown command:" ++ i) >> loopGuts
