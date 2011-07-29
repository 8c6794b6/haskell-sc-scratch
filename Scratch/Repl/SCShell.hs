{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
module SCShell where

import Control.Monad.Trans (liftIO)

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import Sound.SC3
import Sound.SC3.Lepton

import Parser1
import SampleData
import SCZipper

-- |
-- TODO:
--
-- * Add synth protocol and port number in state (default: udp 57110).
-- * Reuse synth connection inside shell.
--
main :: IO ()
main = do
  ini <- (flip SCZipper []) `fmap` withSC3 getRootNode
  runShell shellDesc haskelineBackend ini
  putStrLn "Bye."

shellDesc :: ShellDescription SCZipper
shellDesc = (mkShellDescription cmds work)
  { prompt = \st -> return $ makePrompt st
  , greetingText = Just ("Type ':q' to quit\n") }

makePrompt :: SCZipper -> String
makePrompt z = foldr f "/" ns  ++ g (focus z) ++ " > " where
  ns = filter (\(SCPath n _ _) -> n /= 0) $ reverse $ scPaths z
  f (SCPath n _ _) cs = "/" ++ show n ++ cs
  g n | nodeId n == 0 = ""
      | otherwise     = case n of
        Group gid _     -> show gid
        Synth nid def _ -> show nid ++ "[" ++ def ++ "]"

cmds :: [ShellCommand SCZipper]
cmds = [exitCommand "q"]

-- |
-- TODO:
--
-- Use Parsec and write below commands:
--
-- * DONE, but not so useful: 'pwd'
-- * DONE: 'cd', specifying destination in absolute and relative path.
-- * DONE: 'ls', take optional directory path to show.
-- * DONE: 'tree', show nodes under given path with 'drawSCNode' function.
-- * DONE: 'set', sends n_set OSC command
-- * DONE: 'free', sends n_free OSC command
-- * DONE: 'set', with n_map and n_mapn.
-- * DONE: 'mv', to move around node with specifying new position
-- * 'find', querying node like, find command, or WHERE clause in SQL.
--
work :: String -> Sh SCZipper ()
work cs | null $ dropWhile (== ' ') cs = return ()
        | otherwise = case parseCmd cs of
  Left err  -> shellPutErrLn $ show err
  Right cmd -> case cmd of
    Pwd    -> shellPutStrLn . show =<< getShellSt
    Ls f   -> shellPutStr . showNode . focus . f =<< getShellSt
    Tree f -> shellPutStr . drawSCNode . focus . f =<< getShellSt
    Cd f   -> modifyShellSt f
    Mv act source target -> do
      liftIO $ withSC3 $ flip send $ n_order act source target
      t <- liftIO $ withSC3 getRootNode 
      putShellSt $ SCZipper t []
    Set nid f  -> do
      newNode <- (f . nodeById nid) `fmap` getShellSt
      liftIO $ withSC3 $ setNode newNode
      modifyShellSt $ \st -> insert' newNode AddReplace (nodeId newNode) st
    Free ns -> do
      modifyShellSt (foldr (.) id (map delete ns))
      liftIO $ withSC3 $ flip send $ n_free ns
    New i param -> case param of
      Nothing -> do
        st <- getShellSt
        let st' = insert (Group i []) st
            j = nodeId $ focus st
        putShellSt st'
        liftIO $ withSC3 $ flip send $ g_new [(i,AddToTail,j)]
      Just (name,ps) -> do
        st <- getShellSt
        let newNode = Synth i name ps
            st' = insert newNode st
            j = nodeId $ focus st
        putShellSt st'
        liftIO $ withSC3 $ addNode j newNode
    Run r  -> do
      nid <- (nodeId . focus) `fmap` getShellSt
      liftIO $ withSC3 $ flip send $ n_run [(nid,r)]
    Status -> liftIO $ withSC3 serverStatus >>= mapM_ putStrLn
    Refresh -> do
      t <- liftIO $ withSC3 getRootNode 
      putShellSt $ SCZipper t []

-- | Show synth node in ls command.
showNode :: SCNode -> String
showNode s = case s of
  Synth _ _ ps -> foldr f [] ps
    where f p ps = show p ++ "\n" ++ ps
  Group nid ss -> foldr f [] ss
    where
      f n ns = g n ++ "\n" ++ ns
      g n = case n of
        Group nid _      -> "g:" ++ show nid
        Synth nid name _ -> "s:" ++ show nid ++ "[" ++ name ++ "]"
