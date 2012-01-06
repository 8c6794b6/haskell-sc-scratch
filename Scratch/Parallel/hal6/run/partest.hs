module Main where

import qualified Hal6.ParTest1 as P1
import qualified Hal6.ParTest2 as P2
import qualified Hal6.ParTest3 as P3
import qualified Hal6.ParTest4 as P4
import qualified Hal6.ParTest5 as P5
import qualified Hal6.ParTest6 as P6
import qualified Hal6.ParTest7 as P7

import System.Environment (getArgs)

main :: IO ()
main = do
  (n:_) <- getArgs
  case (read n :: Int) of
    1 -> P1.main
    2 -> P2.main
    3 -> P3.main
    4 -> P4.main
    5 -> P5.main
    6 -> P6.main
    7 -> P7.main
    _ -> print $ "Not implemented: " ++ n
