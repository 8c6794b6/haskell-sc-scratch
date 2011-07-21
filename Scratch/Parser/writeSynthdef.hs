------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
module Main where

import Control.Monad
import System.Environment

import Data.Generics.Uniplate.Operations (universeBi)
import Language.Haskell.Interpreter
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Sound.SC3
import Sound.SC3.Lepton

{-|
Read a haskell source code with UGen definitoin written, and write
synthdef files.

Suppose that, contents of a file MyUGens.hs is shown as below:

> -- MyUGens.hs
> module MyUGens where
> 
> ug001 :: UGen
> ug001 = 
>   out 0 $ sinOsc ar 440 0 * 
>   envGen kr 1 1 0 1 RemoveSynth (envPerc 1e-2 1)
> 
> ug002 :: UGen
> ug002 = 
>   out 0 $ lfPar ar 330 0 * 
>   envGen kr 1 1 0 1 RemoveSynth (envPerc 5e-2 5e-2)

This action will parse this file and write synthdef for ug001 and ug002.
To write synthdef, explicitly put type UGen signature to the function.
-}
main :: IO ()
main = do
  filePath:_ <- getArgs
  pairOfDefs <- runInterpreter $ parseContents filePath
  case pairOfDefs of
    Left err   -> print err
    Right defs -> forM_ defs $ \(name, ug) -> do
      putStr $ "Writing: " ++ name ++ "... "
      writeSynthdef name ug
      putStrLn "done."

parseContents :: FilePath -> Interpreter [(String,UGen)]
parseContents path = do
  (modName, ugs) <- liftIO $ parseIt path
  loadModules [path]
  setTopLevelModules [modName]
  setImports ["Prelude","Sound.SC3","Sound.SC3.UGen","Sound.SC3.Lepton"]
  forM ugs $ \n -> interpret n (as :: UGen) >>= \u -> return (n,u)
  
parseIt :: FilePath -> IO (String,[String])
parseIt path = do
  mod <- parseModule `fmap` readFile path
  case mod of 
    ParseFailed loc err -> print loc >> error err
    ParseOk mod@(HsModule _ (Module moduleName) _ _ _) -> do
      let p (HsTypeSig _ _ 
             (HsQualType _ (HsTyCon (UnQual (HsIdent "UGen"))))) = True
          p _                                                    = False
          f (HsTypeSig _ (HsIdent n:_) _) = n
      return (moduleName, [f exp | exp <- universeBi mod, p exp])