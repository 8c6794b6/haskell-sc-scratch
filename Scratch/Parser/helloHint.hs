module Main where

import Language.Haskell.Interpreter
import Sound.SC3
import Sound.SC3.Lepton

main :: IO ()
main = do
  result <- runInterpreter hints
  case result of 
    Left err -> print err
    Right x -> do
      putStrLn $ "foo is: " 
      putStrLn $ show x
    
hints :: Interpreter UGen
hints = do
  loadModules ["ImportMe.hs"]
  setTopLevelModules ["ImportMe"]
  setImports ["Prelude","Sound.SC3","Sound.SC3.UGen","Sound.SC3.Lepton"]
  interpret "foo" (as :: UGen)
