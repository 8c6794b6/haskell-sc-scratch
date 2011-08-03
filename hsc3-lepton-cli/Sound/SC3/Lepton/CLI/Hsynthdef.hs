{-# LANGUAGE DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : $Header$
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : non-portable (DeriveDataTypeable)
--
module Sound.SC3.Lepton.CLI.Hsynthdef where

import Control.Monad
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
> ug002 :: UGen -> UGen -> UGen -> UGen
> ug002 t_trig amp freq =
>   out 0 $ lfPar ar freq 0 *
>   envGen kr t_trig amp 0 1 RemoveSynth (envPerc 5e-2 5e-2)

This action will parse this file and write synthdef for ug001 and ug002.
To write synthdef, explicitly put type UGen signature to the function.

-}
main :: IO ()
main = do
  Hsynthdefs sInfo path toWrite <- cmdArgs defaultArg
  work toWrite sInfo path

data Hsynthdefs = Hsynthdefs
  { server :: Maybe (String, Int)
  , defSource :: FilePath
  , noWrite :: Bool
  } deriving (Eq,Show,Data,Typeable)

defaultArg :: Hsynthdefs
defaultArg = Hsynthdefs
  { server = Nothing &=
             help "host and port to send d_recv message" &=
             typ "HOST,PORT"
  , noWrite = False &= help "When turned on, won't write file"
  , defSource = def &= args &= typ "FILE" }
  &= summary "read haskell source code containing synthdefs"

work :: Bool -> Maybe (String, Int) -> FilePath -> IO ()
work toWrite hp path = do
  pairOfDefs <- runInterpreter $ parseContents path
  case pairOfDefs of
    Left err   -> print err
    Right defs -> do
      case hp of
        Nothing -> return ()
        Just (host, port) ->
          withTransport (openUDP host port) $ \fd ->
            forM_ defs $ \(n,ug) -> do
              putStr $ "sending " ++ n ++ "... "
              send fd $ d_recv $ synthdef n ug
              putStrLn $ "done."
      unless toWrite $ forM_ defs $ \(n, ug) -> do
        putStr $ "Writing: " ++ n ++ "... "
        writeSynthdef n ug
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
  mdl <- parseModule `fmap` readFile path
  case mdl of
    ParseFailed loc err -> print loc >> error err
    ParseOk mdl'@(HsModule _ (Module moduleName) _ _ _) -> do
      let p (HsTypeSig _ _
             (HsQualType _ (HsTyCon (UnQual (HsIdent "UGen"))))) = True
          p _                                                    = False
          f (HsTypeSig _ (HsIdent n:_) _) = n
          f _ = ""
      return (moduleName, [f e | e <- universeBi mdl', p e])
