{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import qualified Tut.Repa as T

data RepaTut
  = Luminate {inFile :: FilePath, outFile :: FilePath}
  | Rot180 {inFile :: FilePath, outFile :: FilePath}
  | Gradiate {outFile :: FilePath}
  | Bench
  deriving (Show, Data, Typeable)
           
commands :: [RepaTut]
commands =
  [ Luminate 
    { inFile = def &= typFile, outFile = def &= typFile }
    &= help "Luminate given bmp image"
  , Rot180 
    { inFile = def &= typFile, outFile = def &= typFile }
    &= help "Rotate given bmp image for 180 degrees"
  , Gradiate 
    { outFile = def &= typFile &= groupname "repatut gradiate"}
    &= help "Write gradiation image"
  , Bench 
    &= help "Benchmark array operation"
  ]
  
ms :: Mode (CmdArgs RepaTut)
ms = cmdArgsMode $ modes commands  
  
main :: IO ()
main = do
  arguments <- cmdArgsRun ms
  case arguments of
    Luminate inf outf -> T.luminate inf outf
    Rot180 inf outf   -> T.rotimg inf outf
    Gradiate outf     -> T.write_grad' outf
    Bench             -> T.bench_para
