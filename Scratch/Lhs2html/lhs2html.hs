{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Script to parse lhs directory contents to html with same directory structure.
--
module Main where

import System.Directory (doesDirectoryExist)
import System.FilePath (replaceExtension, takeBaseName)

import Language.Haskell.HsColour (Output(..), hscolour)
import Language.Haskell.HsColour.Colourise (defaultColourPrefs)
import System.Console.CmdArgs ((&=))
import System.Directory.Tree (AnchoredDirTree(..))
import qualified System.Console.CmdArgs as C
import qualified System.Directory.Tree as T
import qualified Text.Pandoc as P

main :: IO ()
main = do
  Lhs2Html inF outF <- C.cmdArgs defaultArg
  case (inF,outF) of
    (Just iPath, Just oPath) -> do
      isDir <- doesDirectoryExist iPath
      if isDir
        then convertLhss iPath oPath >> putStrLn "done"
        else readFile iPath >>= writeFile oPath . lhs2html iPath
    (Just iPath, Nothing) -> do
      isDir <- doesDirectoryExist iPath
      if isDir
        then error "Output must be specified"
        else readFile iPath >>= putStr . lhs2html iPath
    (Nothing, Just oPath) ->
        writeFile oPath . lhs2html "stdin" =<< getContents
    (Nothing, Nothing) ->
        putStr . lhs2html "stdin" =<< getContents

-- | Data type for command line arguments.
data Lhs2Html = Lhs2Html
  { input :: Maybe FilePath
  , output :: Maybe FilePath }
  deriving (Eq, Show, C.Data, C.Typeable)

-- | Default for input argument
defaultArg :: Lhs2Html
defaultArg = Lhs2Html
  { input  = C.def &= C.help "input file or directory [default=STDIN]"
  , output = C.def &= C.help "output destination      [default=STDOUT]"}
  &= C.summary "lhs2html: literate haskell code -> html"

-- | Apply lhs2html to all files under given directory.
convertLhss :: FilePath -- ^ Input root directory
            -> FilePath -- ^ Output root directory
            -> IO (AnchoredDirTree ())
convertLhss readRoot writeRoot = do
  _ :/ cont <- T.readDirectory readRoot
  T.writeDirectoryWith
    (\path contents ->
      let outPath = replaceExtension path ".html"
      in  writeFile outPath (lhs2html outPath contents))
    (writeRoot :/ cont)

-- | Converts lhs string to html string.
lhs2html :: String -- ^ Title of output html
         -> String -- ^ Input lhs contents
         -> String -- ^ Colourized and converted html contents
lhs2html title contents =
  (P.writeHtmlString P.defaultWriterOptions {P.writerReferenceLinks=True}) .
  P.readMarkdown P.defaultParserState .
  hscolour ICSS defaultColourPrefs False False (takeBaseName title) True $
  contents