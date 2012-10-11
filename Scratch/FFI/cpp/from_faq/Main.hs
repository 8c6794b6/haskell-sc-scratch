{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : $Header$
Stability   : experimental
Portability : non-portable (FFI)

Example to call C++ class method with thin C wrapper. Compile with
C++ linker, e.g:

   $ ghc --make -pgml g++ Main.hs c-function.c Fred.cpp

-}
module Main where

main :: IO ()
main = do
  putStrLn "Calling c_wilma_with_new_fred"
  c_wilma_with_new_fred

foreign import ccall safe "Fred.h"
  c_wilma_with_new_fred :: IO ()