module Main where

import Etude.Additive01 (goSend, n2)
import Control.Monad (mapM_)

main = mapM_ goSend n2