{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Henc - haskell expression node count, a simple CLI for haskell-src-codesize.
-}
module Main where

import System.Environment (getArgs)
import System.Console.GetOpt
import Language.Haskell.Exts

import Language.Haskell.CodeSize

data Flag
  = Exts [Extension]
  | Help
  deriving (Eq,Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "Show this help"
  , Option ['x'] ["ext"]
    (ReqArg (Exts . map read . commas) "EXT1,EXT2,...")
    "Explicitly specify lang extensions"
  ]

commas :: String -> [String]
commas str = go str where
  go str'
    | null str' = []
    | otherwise = pre : go (dropWhile (== ',') post) where
      (pre, post) = span (/= ',') str'

usage :: IO ()
usage =
  putStrLn $ usageInfo "Usage: henc [OPTIONS] [FILEPATH]\n\nOPTIONS:" options

work :: [String] -> IO ()
work argv = case getOpt Permute options argv of
  (os,fs,[])
    | Help `elem` os -> usage
    | null fs   -> do
      getContents >>= print . count
    | otherwise -> do
      let filepath = head fs
      n <- count $ src filepath
      putStrLn $ unwords [show n, "\t", filepath]
  (_,_,errs)  -> do
    putStrLn (unwords errs)
    usage

main :: IO ()
main = work =<< getArgs