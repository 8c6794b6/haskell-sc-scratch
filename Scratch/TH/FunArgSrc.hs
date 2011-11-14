{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Attempt to extract argument string passed to function with th.
Source code side of Template haskell code.

-}
module FunArgSrc where

import Data.Generics
import Control.Monad
import Language.Haskell.TH hiding (Match)

import Language.Haskell.Exts
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

import qualified Language.Haskell.Exts as E
import qualified Language.Haskell.TH as TH

-- import Language.Haskell.Syntax

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
  
funArgs :: TH.Name -> ExpQ
funArgs fname = do
  path <- loc_filename `fmap` location
  ParseOk mdl <- runIO $ parseFileWithMode (defaultParseMode) path
  let fname' = nameBase fname
      args = extractArgs fname' mdl
      extractArgs fname mdl = everything mplus ([] `mkQ` f) mdl
      f :: Match -> [String]
      f (Match _ (Ident idnt) ps _ _ _) 
        | idnt == fname' = map unPVar ps
        | otherwise       = []
      unPVar (PVar n) = case n of Ident i -> i; _ -> ""
  TH.listE (map (litE . stringL) args)
  
  
extractArgs :: String -> Module -> [String]
extractArgs fname mdl = everything mplus ([] `mkQ` f) mdl where
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
  
  -- cnt <- runIO $ readFile path
  -- let mdl = show $ parseWithExts cnt
  -- [| mdl |]
  
  
showQ :: Show a => Q a -> IO ()
showQ x = putStrLn =<< (runQ $ show `fmap` x)
