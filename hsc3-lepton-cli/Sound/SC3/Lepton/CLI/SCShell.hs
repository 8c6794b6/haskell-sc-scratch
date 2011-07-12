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
-- Note that, haskeline backend used in this module is customized.
-- Modified mtl version in dependency list of Shellac-haskeline's cabal file
-- from 1.1.1.1 to 2.0.1.
--
-- TODO:
--
-- * Add completion helpers
--
-- * Add help command and show example
--
-- * Show exception, e.g. duplicate node id
--
-- * Add 'find' command, query nodes like find or WHERE in SQL.
--
-- * Make 'pwd' a bit more useful ... any idea?
--
module Sound.SC3.Lepton.CLI.SCShell where

import Control.Monad.Trans (liftIO)
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import Sound.SC3.Lepton.CLI.Parser
import Sound.SC3.Lepton.CLI.SCShellCmd
import Sound.SC3.Lepton.CLI.SCZipper

data SynthEnv = SynthEnv
  { connection :: Either TCP UDP
  , zipper :: SCZipper }
  deriving (Eq, Show)

instance Transport (Either TCP UDP) where
  send (Left t) m  = send t m
  send (Right t) m = send t m
  recv (Left t)  = recv t
  recv (Right t) = recv t
  close (Left t)  = close t
  close (Right t) = close t

-- | Entry point for interactive prompt.
scShell :: Either TCP UDP -> IO ()
scShell con = do
  n <- getRootNode con
  runShell shellDesc haskelineBackend $ SynthEnv con (SCZipper n [])
  putStrLn "Bye."

withEnv :: (Either TCP UDP -> IO a) -> Sh SynthEnv a
withEnv action = liftIO . action . connection =<< getShellSt

shellDesc :: ShellDescription SynthEnv
shellDesc = (mkShellDescription cmds work)
  { prompt = \st -> return $ makePrompt st
  , greetingText = Just ("Type ':q' to quit\n") }

makePrompt :: SynthEnv -> String
makePrompt e = foldr f "/" ns  ++ g (focus z) ++ " > " where
  z = zipper e
  ns = filter (\(SCPath n _ _) -> n /= 0) $ reverse $ scPaths z
  f (SCPath n _ _) cs = "/" ++ show n ++ cs
  g n | nodeId n == 0 = ""
      | otherwise     = case n of
        Group gid _     -> show gid
        Synth nid def _ -> show nid ++ "[" ++ def ++ "]"

cmds :: [ShellCommand st]
cmds = [exitCommand "q", helpCommand "h"]

-- | Parse input string, respond to parsed Cmd result with action.
--
work :: String -> Sh SynthEnv ()
work cs | null $ dropWhile (== ' ') cs = return ()
        | otherwise = case parseCmd cs of
  Left err  -> shellPutErrLn $ show err
  Right parsed -> do
    now <- liftIO utcr
    res <- (Bundle (UTCr now) . cmdToOSC parsed . zipper) `fmap` getShellSt
    case parsed of
      Pwd    -> shellPutStrLn . show . zipper =<< getShellSt
      Ls f   -> shellPutStr . showNode . focus . steps f . zipper =<< getShellSt
      Tree f ->
        shellPutStr . drawSCNode . focus . steps f . zipper =<< getShellSt
      Cd f   -> modifyShellSt $ \st -> st {zipper = steps f $ zipper st}
      Status -> withEnv serverStatus >>= mapM_ shellPutStrLn
      -- Mv addAct source target -> do
      Mv _ _ _ -> do
        n <- withEnv $ \fd ->
          send fd res >> wait fd "/done" >> getRootNode fd
                              -- n_order addAct source target
        -- n <- withEnv getRootNode
        modifyShellSt $ \st -> st {zipper = SCZipper n []}
      Set nid ps  -> do
        e <- getShellSt
        let newNode = updateParams ps . nodeById nid . zipper $ e
            z = zipper e
        putShellSt $ e {zipper = insert' newNode AddReplace (nodeId newNode) z}
        withEnv $ setNode newNode
      Free nodeIds -> do
        modifyShellSt $ \st ->
          st {zipper = (foldr (.) id (map delete nodeIds)) $ zipper st}
        withEnv $ flip send $ res -- n_free nodeIds
      New i nodeType -> case nodeType of
        Nothing -> do
          st <- getShellSt
          let st' = st {zipper = insert (Group i []) z}
              -- j = nodeId $ focus z
              z = zipper st
          putShellSt st'
          withEnv $ flip send $ res -- g_new [(i,AddToTail,j)]
        Just (n,ps) -> do
          st <- getShellSt
          let newNode = Synth i n ps
              st' = st {zipper = insert newNode z}
              j = nodeId $ focus z
              z = zipper st
          putShellSt st'
          withEnv $ addNode j newNode
      Run _  -> do
        -- nid <- (nodeId . focus . zipper) `fmap` getShellSt
        withEnv $ flip send $ res -- n_run [(nid,bool)]
      Refresh -> do
        n <- withEnv getRootNode
        modifyShellSt $ \st -> st {zipper = SCZipper n []}

-- | Show synth node in ls command.
showNode :: SCNode -> String
showNode nd = case nd of
  Synth _ _ ps -> foldr f [] ps
    where f x xs = show x ++ "\n" ++ xs
  Group _ ss -> foldr f [] ss
    where
      f n ns = g n ++ "\n" ++ ns
      g n = case n of
        Group i _   -> "g:" ++ show i
        Synth i defName _ -> "s:" ++ show i ++ "[" ++ defName ++ "]"
