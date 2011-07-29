------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Playing with Shellac package.
--
module Repl00 where

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

main :: IO ()
main = do 
  putStrLn "Type ':h' for help, ':q' to quit"
  n <- runShell (mkShellDescription cmds work) haskelineBackend 0
  putStrLn $ "You've entered " ++ show n ++ " line" ++ 
    (if n > 1 then "s" else "") ++ "."
  putStrLn $ "Bye!"
    
cmds :: [ShellCommand Int]
cmds = [exitCommand "q", helpCommand "h"]
  
work :: String -> Sh Int ()
work [] = return ()
work x  = modifyShellSt (+1) >> shellPutStrLn x
