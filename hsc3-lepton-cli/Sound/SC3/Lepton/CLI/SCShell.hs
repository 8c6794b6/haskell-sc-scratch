{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Interactive shell for scsynth nodes.
--
-- TODO:
--
-- * Make target nodeId for set commnad optional, use current node as default.
--
-- * Take node ids for new command to make multiple group nodes at once.
--
-- * Add completion helpers
--
--     * For synthdef name
--
--     * For synthdef params -> Depends on synthdef parser to get controls.
--
-- * Add help command
--
-- * Show exception, e.g. duplicate node id ... what can we detect?
--
-- * Add 'find' command, query nodes like find or WHERE in SQL, do something
--   with matching nodes.
--
-- * Make 'pwd' a bit more useful ... any idea?
--
module Sound.SC3.Lepton.CLI.SCShell where

import Control.Applicative
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, sort)
import System.FilePath

import Control.Monad.State
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.Haskeline

import Sound.SC3.Lepton.CLI.Parser
import Sound.SC3.Lepton.CLI.SCShellCmd
import Sound.SC3.Lepton.CLI.SCZipper

-- | Environment for scshell.
data Env = Env
  { -- | Scsynth connection used inside the shell.
    connection :: Either TCP UDP
    -- | Zipper data to hold current position in synth tree.
  , zipper :: SCZipper
  } deriving (Eq, Show)

-- | Wrapper for inner guts of repl.
newtype Guts s a = Guts {
  unGuts :: StateT s IO a
  } deriving (Applicative, Monad, Functor, MonadException, MonadIO, MonadState s)

-- | Read-Eval-Print-Loop using haskeline with guts.
type Repl a = InputT (Guts Env) a

instance Transport (Either TCP UDP) where
  send (Left t) m  = send t m
  send (Right t) m = send t m
  recv (Left t)  = recv t
  recv (Right t) = recv t
  close (Left t)  = close t
  close (Right t) = close t

getEnv :: Repl Env
getEnv = lift get

putEnv :: Env -> Repl ()
putEnv = lift . put

modifyEnv :: (Env -> Env) -> Repl ()
modifyEnv f = getEnv >>= putEnv . f

-- | Lifts actions using scsynth connection to Repl.
withEnv :: (Either TCP UDP -> IO a) -> Repl a
withEnv action = liftIO . action . connection =<< getEnv

go :: IO ()
go = do
  let con = Right <$> openUDP "127.0.0.1" 57110 :: IO (Either TCP UDP)
  withTransport con scShell

-- | Entry point. Starts repl interpreter with given connection.
scShell :: Either TCP UDP -> IO ()
scShell con = do
  n <- getRootNode con
  evalStateT (unGuts (runInputT (setComplete compTop defaultSettings) repl))
    (Env con (SCZipper n []))

-- | The loop.
repl :: Repl ()
repl = do
  input <- getInputLine . makePrompt =<< getEnv
  case input of
    Nothing -> repl
    Just i -> case trim i of
      ""     -> repl
      "quit" -> outputStrLn "bye"
      _      -> work (trim i) >> repl

-- | Parse command string and sends message, update current repl state.
work :: String -> Repl ()
work cs = case parseCmd cs of
  Left err  -> outputStrLn $ show err
  Right parsed -> do
    now <- liftIO utcr
    res <- (Bundle (UTCr now) . cmdToOSC parsed . zipper) `fmap` getEnv
    case parsed of
      Pwd    -> outputStrLn . show . zipper =<< getEnv
      Ls f   -> outputStr . showNode . focus . steps f . zipper =<< getEnv
      Tree f -> outputStr . drawSCNode . focus . steps f . zipper =<< getEnv
      Cd f   -> modifyEnv $ \st -> st {zipper = steps f $ zipper st}
      Status -> withEnv dumpStatus
      Mv a i j -> do
        withEnv $ flip send res
        modifyEnv $ \st -> st {zipper = move i a j $ zipper st}
      Set nid ps  -> do
        e <- getEnv
        let newNode = updateParams ps . nodeById nid . zipper $ e
            z = zipper e
        putEnv $ e {zipper = insert' newNode AddReplace (nodeId newNode) z}
        withEnv $ setNode newNode
      Free nids -> do
        modifyEnv $ \st ->
          st {zipper = (foldr (.) id (map delete nids)) $ zipper st}
        withEnv $ flip send res
      New i detail -> case detail of
        Nothing -> do
          modifyEnv $ \st -> st {zipper = insert (Group i []) (zipper st)}
          withEnv $ flip send res
        Just (n,ps) -> do
          st <- getEnv
          let newNode = Synth i n ps
              st' = st {zipper = insert newNode z}
              j = nodeId $ focus z
              z = zipper st
          putEnv st'
          withEnv $ addNode j newNode
      Run _  -> withEnv $ flip send $ res
      Refresh -> do
        n <- withEnv getRootNode
        modifyEnv $ \st -> st {zipper = SCZipper n []}

-- | Complete function for toplevel commands.
compTop :: CompletionFunc (Guts Env)
compTop = completeWordWithPrev Nothing " \t" $ \left current -> do
  e <- get
  let lefts = words left
  case lefts of
    (cmd:_) -> return $ compArgs e (reverse cmd) left current
    []      -> return [ simpleCompletion t
                      | t <- toplevels, current `isPrefixOf` t]

-- | Complete argument for each commands.
compArgs :: Env -> String -> String -> String -> [Completion]
compArgs e com rest current
  | com `elem` ["cd", "ls", "tree"] = compPaths e current
  | otherwise                       = []

-- | Make list of canditate paths from current environment and input.
compPaths :: Env -> String -> [Completion]
compPaths e current =
  let (pre,post) = splitFileName current
      moves = case parsePaths pre of Right ps -> ps; Left _ -> []
      cwn = focus . steps moves . zipper $ e
  in  case cwn of
    Synth _ _ _ -> []
    Group _ ns  ->
      [comp | n <- ns, let n' = shortPath n
            , post `isPrefixOf` n'
            , let repl = replaceFileName current (addTrailingSlash n')
            , let comp = Completion repl (middlePath n) False]

addTrailingSlash :: String -> String
addTrailingSlash cs | "/" `isSuffixOf` cs = cs
                    | otherwise           = cs ++ "/"

-- | Show node id for synth nodes, nd node id ++ '/' for group nodes.
shortPath (Synth i _ _) = show i
shortPath (Group i _)   = show i ++ "/"

-- | Show node id and defname for synth nodes, node id ++ '/' for group nodes.
middlePath (Synth i n _) = show i ++ ":" ++ n
middlePath (Group i _)   = show i ++ "/"

-- | Name of toplevel commands.
toplevels :: [String]
toplevels = sort $
  ["quit", "pwd", "ls", "tree", "cd", "status", "mv", "set", "free", "new"
  ,"run", "refresh"]

-- | Make prompt string showing current node from env.
makePrompt :: Env -> String
makePrompt e = foldr f "/" ns  ++ g (focus z) ++ " > " where
  z = zipper e
  ns = filter (\(SCPath n _ _) -> n /= 0) $ reverse $ scPaths z
  f (SCPath n _ _) cs = "/" ++ show n ++ cs
  g n | nodeId n == 0 = ""
      | otherwise     = case n of
        Group gid _     -> show gid
        Synth nid def _ -> show nid ++ ":" ++ def

-- | Show synth node in ls command.
showNode :: SCNode -> String
showNode nd = case nd of
  Synth _ _ ps -> foldr (\x xs -> show x ++ "\t" ++ xs) [] ps ++ "\n"
  Group _ ss -> foldr (\x xs -> middlePath x ++ "\t" ++ xs) [] ss ++ "\n"

-- | Removes space characters in beginning and end of given String.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
