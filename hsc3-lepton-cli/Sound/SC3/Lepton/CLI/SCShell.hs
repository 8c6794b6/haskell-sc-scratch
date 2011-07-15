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
-- * Add completion helpers
--
--     * For synthdef params -> Depends on synthdef parser to get controls.
--
-- * Print synth nodes with using pretty print package.
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
import Data.Maybe (fromMaybe)
import System.Directory (getDirectoryContents)
import System.FilePath
  (splitFileName, replaceFileName, dropExtension, takeExtension)
import Text.PrettyPrint

import Control.Monad.State
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.Haskeline

import Sound.SC3.Lepton.CLI.Parser
import Sound.SC3.Lepton.CLI.SCShellCmd
import Sound.SC3.Lepton.CLI.SCZipper

import qualified System.Environment as E

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

modifyZipper :: (SCZipper -> SCZipper) -> Repl ()
modifyZipper f = modifyEnv (\e -> e {zipper = f $ zipper e})

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
    let sendIt = withEnv (flip send res)
    case parsed of
      Pwd    -> outputStrLn . show . zipper =<< getEnv
      Ls f   -> outputStr . showNode . focus . steps f . zipper =<< getEnv
      Tree f -> outputStrLn . renderNode . focus . steps f . zipper =<< getEnv
      Cd f   -> modifyEnv $ \st -> st {zipper = steps f $ zipper st}
      Status -> withEnv dumpStatus
      Mv a i j -> sendIt >> modifyZipper (move i a j)
      Set nid ps  -> do
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
  case lefts of
    (cmd:_) -> compArgs e (reverse cmd) left current
    []      -> return [ simpleCompletion t
                      | t <- toplevels, current `isPrefixOf` t]

-- | Complete argument for each commands.
compArgs :: Env -> String -> String -> String -> Guts Env [Completion]
compArgs e com left current
  | com `elem` ["cd", "ls", "tree"] = return $ compPaths e current
  | com == "snew"                   = compSynthdefName e current
  | otherwise                       = return []

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
compSynthdefName :: Env -> String -> Guts Env [Completion]
compSynthdefName e current = do
  sdefs <- liftIO $ synthdefs
  return [simpleCompletion d | d <- sdefs, current `isPrefixOf` d]

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
toplevels = sort $
  ["quit", "pwd", "ls", "tree", "cd", "status", "mv", "set", "free"
  ,"snew", "gnew", "run", "refresh"]

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

-- | Synthdefs files in SC_SYNTHDEF_PATH, without extension.
synthdefs :: IO [String]
synthdefs =
  E.getEnv scSynthdefPath >>= getDirectoryContents >>=
  return . map dropExtension . filter (`notElem` [".",".."])

-- | Removes space characters in beginning and end of given String.
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Pretty print SCNode.
renderNode :: SCNode -> String
renderNode = render . prettyNode

prettyNode :: SCNode -> Doc
prettyNode n = case n of
  Group i ns   -> int i <+> text "group" $$ vcat (map (nest 3 . prettyNode) ns)
  Synth i n ps -> int i <+> text n $$ hsep (map (nest 2 . prettyParam) ps)

prettyParam :: SynthParam -> Doc
prettyParam p = case p of
  n:=v  -> text n <> char ':' <+> double v
  n:<-v -> text n <> char ':' <+> char 'c' <> int v
  n:<=v -> text n <> char ':' <+> char 'a' <> int v