{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : non-portable (TemplateHaskell)

Attempt to extract argument string passed to function with th.
Source code side of Template haskell code.

Caller module may look like:

> main :: IO ()
> main = putStrLn $ unwords $
>   ["Argument of", show 'foo ++ ":"] ++ $(funArgs 'foo)
>
> foo :: Int -> Int -> Int -> Int
> foo apple banana cherry = (apple + banana) * cherry

Invoking main will show below:

> ghci> main
> Argument of FunArgRun.foo: apple banana cherry

-}
module FunArgSrc where

import Control.Monad (when)
import Data.Generics (everything, mkQ)
import Language.Haskell.Exts
import Language.Haskell.TH hiding (Match)
import System.Directory (doesFileExist)
import qualified Language.Haskell.Exts as E
import qualified Language.Haskell.TH as TH

-- | Extracts function arguments.
--
-- Returned 'ExpQ' is `[String]` containing function names bounded to
-- given function. Requires source code of caller to be accessible.
--
funArgs :: TH.Name -> ExpQ
funArgs fname = do

  -- Need to access the source code of caller, ensuring existence.
  -- Though, does the caller file vanish during the execution?
  --
  path <- loc_filename `fmap` location
  exists <- runIO $ doesFileExist path
  when (not exists) $ error $ "Cannot access " ++ path

  -- Actual worker to parse the source contents.
  --
  -- It is unlikely to fail parsing, since when the caller code
  -- are able to compile, most time it is able to parse. Otherwise
  -- it seems like a bug in parseFile.
  --
  result <- runIO $ parseFileWithMode defaultParseMode path
  case result of
    ParseFailed l e -> do
      runIO $ putStrLn $ unwords ["Failed to parse", show l, e]
      TH.listE []
    ParseOk mdl -> do
      let fname' = nameBase fname
          args = everything (++) ([] `mkQ` f) mdl
          f (Match _ (Ident idnt) ps _ _ _)
            | idnt == fname' = map unPVar ps
            | otherwise      = []
          unPVar (PVar n) = case n of Ident i -> i; _ -> ""
      if null args
         then do
            -- Show message for (probably) useless call, which returns empty
            -- list. Is there a use case for caller to check whether a
            -- function takes args or not?
            runIO $ do
              putStrLn $ "Argument not passed to '" ++ show fname ++ "'"
              putStrLn "Probably you don't need to call funArgs"
            TH.listE []
         else
            TH.listE (map (litE . stringL) args)

myLocation :: ExpQ
myLocation = do
  path <- loc_filename `fmap` location
  [| path |]

myContents :: ExpQ
myContents = do
  path <- loc_filename `fmap` location
  contents <- runIO $ readFile path
  [| contents |]

-- myModule :: ExpQ
myModule = do
  path <- loc_filename `fmap` location
  ParseOk mdl <- runIO $ parseFileWithMode (defaultParseMode) path
  let args = extractArgs "foo" mdl
  [| show args |]
  -- [| mdl |]

extractArgs :: String -> Module -> [String]
extractArgs fname mdl = everything (++) ([] `mkQ` f) mdl where
  f :: Match -> [String]
  f (Match _ (Ident fname') ps _ _ _)
    | fname == fname' = map unPVar ps
    | otherwise       = []
  unPVar (PVar n) = case n of Ident i -> i; _ -> ""

{-

FunBind
 [ Match
   { srcFilename = ..., srcLine = .. , srcColumn = .. }
   (Ident "foo")
   [ PVar (Ident "arg1")
   , PVar (Ident "arg2")
   , PVar (Ident "arg3") ]
   Nothing
   (UnGuardedRhs ..)
 , .... ]

-}
