module Main where

import qualified Self.Dep.Foo

main :: IO ()
main = print Self.Dep.Foo.foo
