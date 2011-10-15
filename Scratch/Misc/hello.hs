module Main where

import Control.Monad
import qualified Codec.Compression.Snappy.Lazy as S
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = do
  cts <- C8.readFile "/usr/share/dict/words"
  putStrLn $ "length of uncompressed: " ++ show (C8.length cts)
  putStrLn $ "length of compressed: " ++ show (C8.length $ S.compress cts)
