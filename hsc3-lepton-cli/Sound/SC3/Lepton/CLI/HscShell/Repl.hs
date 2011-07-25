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
-- * Add help command
--
-- * Show exception, e.g. duplicate node id ... what can we detect?
--
-- * Add 'find' command, query nodes like find or WHERE in SQL, do something
--   with matching nodes ...  what can we do? how?
--
module Sound.SC3.Lepton.CLI.HscShell.Repl where

import Control.Applicative hiding (empty)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), splitFileName, replaceFileName, dropExtension)

import Control.Monad.State
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.Haskeline

import Sound.SC3.Lepton.CLI.HscShell.Parser
import Sound.SC3.Lepton.CLI.HscShell.Cmd
import Sound.SC3.Lepton.CLI.HscShell.SynthdefPeeker

import qualified System.Environment as E

-- | Environment for scshell.
data Env = Env
  { -- | Scsynth connection used inside the shell.
    connection :: Either TCP UDP
    -- | Host
  , envHost :: String
    -- | Port
  , envPort :: Int
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

modifyZipper :: (SCZipper -> SCZipper) -> Repl ()
modifyZipper f = modifyEnv (\e -> e {zipper = f $ zipper e})

-- | Lifts actions using scsynth connection to Repl.
withEnv :: (Either TCP UDP -> IO a) -> Repl a
withEnv action = liftIO . action . connection =<< getEnv

go :: IO ()
go = do
  let con = Right <$> openUDP "127.0.0.1" 57110 :: IO (Either TCP UDP)
  withTransport con (scShell "127.0.0.1" 57110)

-- | Entry point. Starts repl interpreter with given connection.
scShell :: String -> Int -> Either TCP UDP -> IO ()
scShell host port con = do
  n <- getRootNode con
  evalStateT (unGuts (runInputT (setComplete compTop defaultSettings) repl))
    (Env con host port (SCZipper n []))

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
    let sendIt = withEnv (flip send res)
    case parsed of
      Pwd      -> outputStrLn . show . zipper =<< getEnv
      Ls f     -> outputStr . showNode . focus . steps f . zipper =<< getEnv
      Tree v f ->
        outputStrLn . renderNode v . focus . steps f . zipper =<< getEnv
      Cd f     -> modifyEnv $ \st -> st {zipper = steps f $ zipper st}
      Status   -> withEnv dumpStatus
      Mv a i j -> sendIt >> modifyZipper (move i a j)
      Set nid ps -> do
        e <- getEnv
        let nid' = fromMaybe (nodeId . focus $ z) nid
            newNode = updateParams ps . nodeById nid' . zipper $ e
            z = zipper e
        putEnv $ e {zipper = insert' newNode (Just (AddReplace,nid')) z}
        sendIt
      Free is -> sendIt >> modifyZipper (foldr (.) id (map delete is))
      Snew n i aj ps -> sendIt >> modifyZipper (insert' (Synth i n ps) aj)
      Gnew ps        -> sendIt >> modifyZipper (addGroups ps)
      Run _ -> sendIt
      Refresh ->
        withEnv getRootNode >>= \n -> modifyZipper (const $ SCZipper n [])

-- | Add nodes to given zipper.
addGroups :: [(NodeId, Maybe (AddAction,NodeId))] -> SCZipper -> SCZipper
addGroups ps z = foldr (\(i,aj) -> insert' (Group i []) aj) z (reverse ps)

-- | Complete function for toplevel commands.
compTop :: CompletionFunc (Guts Env)
compTop = completeWordWithPrev Nothing " \t" $ \left current -> do
  e <- get
  let lefts = words left
  if null lefts
    then return [simpleCompletion t | t <- toplevels, current `isPrefixOf` t]
    else compArgs e (reverse (last lefts)) left current

-- | Complete argument for each commands.
compArgs :: Env -> String -> String -> String -> Guts Env [Completion]
compArgs e com left current
  | com `elem` ["cd", "ls"] = return $ compPaths e current
  | com == "tree"           = return $ compPaths e current
  | com == "set"            = compNset current
  | com == "snew"           = compSnew left current
  | otherwise               = return []

-- | Complete arguments for nset command.
compNset :: String -> Guts Env [Completion]
compNset current = do
  e <- get
  let def = case focus $ zipper e of Group _ _ -> ""; Synth _ n _ -> n;
  compParamName def current

-- | Complete arguments for snew command.
compSnew :: String -> String -> Guts Env [Completion]
compSnew left current = do
  let lefts = words left
      wordlen = length lefts
      def = if wordlen >= 2 then reverse (lefts !! (wordlen-2)) else ""
  case wordlen of
    0 -> return []
    1 -> compSynthdefName current
    2 -> return []
    _ -> compParamName def current

-- | Make list of canditate paths from current environment and input.
compPaths :: Env -> String -> [Completion]
compPaths e current =
  let (pre,post) = splitFileName current
      moves = either (const []) id $ parsePaths pre
      cwn = focus . steps moves . zipper $ e
  in  case cwn of
    Synth _ _ _ -> []
    Group _ ns  ->
      [comp | n <- ns, let n' = shortPath n
            , post `isPrefixOf` n'
            , let filled = replaceFileName current (addTrailingSlash n')
            , let comp = Completion filled (middlePath n) False]

-- | Complete synthdef name from synthdefs in SC_SYNTHDEF_PATH environment
-- variable.
compSynthdefName :: String -> Guts Env [Completion]
compSynthdefName current = do
  defs <- liftIO $ synthdefs
  return [simpleCompletion def | def <- defs, current `isPrefixOf` def]

-- | Complete param name, by seeking synthdef file.
compParamName :: String -- ^ Synthdef name
              -> String -- ^ Current input
              -> Guts Env [Completion]
compParamName def current
  | null def  = return []
  | otherwise = do
    filename <- liftIO $ E.getEnv scSynthdefPath >>=
                \d -> return (d </> def ++ ".scsyndef")
    exists <- liftIO $ doesFileExist filename
    if not exists
       then return []
       else liftIO $ return . map (\n -> Completion (n++"=") n False) .
            filter (current `isPrefixOf`) =<< getParamNames filename

-- | Add trailing slash. Won't add when then given string ends with slash.
addTrailingSlash :: String -> String
addTrailingSlash cs | "/" `isSuffixOf` cs = cs
                    | otherwise           = cs ++ "/"

-- | Show node id for synth nodes, nd node id ++ '/' for group nodes.
shortPath :: SCNode -> String
shortPath (Synth i _ _) = show i
shortPath (Group i _)   = show i ++ "/"

-- | Show node id and defname for synth nodes, node id ++ '/' for group nodes.
middlePath :: SCNode -> String
middlePath (Synth i n _) = show i ++ ":" ++ n
middlePath (Group i _)   = show i ++ "/"

-- | Name of toplevel commands.
toplevels :: [String]
toplevels =
  ["cd","free","gnew","ls","mv","pwd","quit","refresh","run","set","snew"
  ,"status","tree"]

-- | Make prompt string showing current node from env.
makePrompt :: Env -> String
makePrompt e = pre ++ foldr f "/" ns  ++ g (focus z) ++ "] > " where
  pre = concat ["sc@", envHost e, ":", show (envPort e), " ["]
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
  Synth _ _ ps -> foldr (\x xs -> showParam x ++ "\t" ++ xs) [] ps ++ "\n"
  Group _ ss -> foldr (\x xs -> middlePath x ++ "\t" ++ xs) [] ss ++ "\n"
  where
    showParam (k:=v) = k ++ ":=" ++ show v
    showParam (k:<-v) = k ++ ":=c" ++ show v
    showParam (k:<=v) = k ++ ":=a" ++ show v

-- | Synthdefs files in SC_SYNTHDEF_PATH, without extension.
synthdefs :: IO [String]
synthdefs =
  E.getEnv scSynthdefPath >>= getDirectoryContents >>=
  return . sort . map dropExtension . filter (`notElem` [".",".."])

-- | Removes space characters in beginning and end of given String.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
