{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Data
import Data.List (intercalate)
import Data.Map (Map)

import Data.Generics.Uniplate.Operations (universeBi)
import Language.Haskell.Interpreter
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.Lepton
import System.Console.CmdArgs

import qualified Data.Map as M

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
  ReadSynthdefs sInfo path toWrite <- cmdArgs defaultArg
  work toWrite sInfo path

data ReadSynthdefs = ReadSynthdefs
  { server :: Maybe (String, Int)
  , defSource :: FilePath
  , noWrite :: Bool
  } deriving (Eq,Show,Data,Typeable)

defaultArg :: ReadSynthdefs
defaultArg = ReadSynthdefs
  { server = Nothing &=
             help "host and port to send d_recv message" &=
             typ "HOST,PORT"
  , noWrite = False &= help "When turned on, won't write file"
  , defSource = def &= help "input source file" }
  &= summary "read haskell source code containing synthdefs"

work :: Bool -> Maybe (String, Int) -> FilePath -> IO ()
work toWrite hp path = do
  pairOfDefs <- runInterpreter $ parseContents2 path
  case pairOfDefs of
    Left err   -> print err
    Right defs -> do
      case hp of
        Nothing -> return ()
        Just (host, port) ->
          withTransport (openUDP host port) $ \fd ->
            forM_ defs $ \(name,ug) -> do
              putStr $ "sending " ++ name ++ "... "
              send fd $ d_recv $ synthdef name ug
              putStrLn $ "done."
      unless toWrite $ forM_ defs $ \(name, ug) -> do
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

parseContents2 :: FilePath -> Interpreter [(String,UGen)]
parseContents2 path = do
  (modName, ugs) <- liftIO $ parseIt2 path
  loadModules [path]
  setTopLevelModules [modName]
  setImports ["Prelude","Sound.SC3","Sound.SC3.UGen","Sound.SC3.Lepton"]
  forM (M.toList ugs) $ \(n,ps) -> do
    let ugString = intercalate " " (n : map (\p -> "(\""++p++"\"@@0)") ps)
    u <- interpret ugString (as :: UGen)
    return (n,u)

parseIt2 :: FilePath -> IO (String,Map String [String])
parseIt2 path = do
  mdl <- parseModule `fmap` readFile path
  case mdl of
    ParseFailed loc err -> print loc >> error err
    ParseOk mdl'@(HsModule _ (Module moduleName) _ _ _) -> do
      let p (HsTypeSig _ _ (HsQualType _ t)) = isUGenType t
          p _                 = False
          f (HsTypeSig _ (HsIdent n:_) _) = n
          g (HsMatch _ (HsIdent fname) ps _ _) = (fname, params) where
                params = [h e | e <- universeBi ps]
                h (HsIdent n) = trim n
                q (HsPVar _) = True
                q _ = False
          trim = filter (`notElem` "'")
          ugNames = [f e | e <- universeBi mdl', p e]
          params = [g e | e <- universeBi mdl']
          tm = M.fromList $ zip ugNames $ repeat []
          pm = M.fromList $ params
      return (moduleName,
              M.filterWithKey (\k _ -> M.member k tm) $ M.union pm tm)

isUGenType :: HsType -> Bool
isUGenType (HsTyCon (UnQual (HsIdent "UGen"))) = True
isUGenType (HsTyFun (HsTyCon (UnQual (HsIdent "UGen"))) t) = isUGenType t
isUGenType _ = False
