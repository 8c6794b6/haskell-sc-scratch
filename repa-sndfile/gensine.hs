module Main where

import System.Environment (getArgs)
import Test.GenSine

main :: IO ()
main = do
  args <- getArgs
  case args of 
    dur:frq:path:_ -> writeSine (read dur) (read frq) path
    _              -> error "Usage: duration freq path"