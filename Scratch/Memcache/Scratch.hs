{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Demo code from memcached github.

-}
module Scratch where

import System.Random

import Network.Memcache (Memcache)
import Network.Memcache.Serializable (Serializable(..))
import qualified Data.ByteString.Char8 as B
import qualified Network.Memcache as MC
import qualified Network.Memcache.Protocol as P

main :: IO ()
main = do
  server <- P.connect "127.0.0.1" 11211
  simpleDemo server
  serializeDemo server
  goRandom server
  P.disconnect server
  
simpleDemo :: (Memcache mc) => mc -> IO ()  
simpleDemo mc = do
  let foo = 3 :: Int
  success <- MC.set mc "foo" foo
  putStrLn $ "Setting foo => 3: " ++ show success ++ "."
  foo' <- MC.get mc "foo"
  case foo' of
    Nothing -> putStrLn "Retreiving foo: expired from cache?"
    Just v  -> putStrLn $ "Cached value for foo is: " ++ show (v::Int) ++ "."
  
data User = User
  { username :: String
  , fontsize :: Int
  } deriving (Eq,Show)
    
instance Serializable User where
  serialize (User n f) = B.pack (n ++ " " ++ show f)
  deserialize str = case B.words str of
    (n:f:[]) -> Just $ User (B.unpack n) (read $ B.unpack f)
    _        -> Nothing

serializeDemo :: Memcache mc => mc -> IO ()  
serializeDemo mc = do
  let fred = User "fred" 24
  MC.set mc "u:fred" fred
  fred' <- MC.get mc "u:fred"
  putStrLn $ "Fred is " ++ show (fred' :: Maybe User)
  invalid <- MC.get mc "this key does not exist"
  putStrLn $ "Unknown returns: " ++ show (invalid :: Maybe User)

goRandom :: Memcache m => m -> IO ()
goRandom mc = do
  g <- newStdGen
  let mkv _ [] = []
      mkv n vs = w : mkv n ws where (w,ws) = splitAt n vs
      keys = mkv 100 $ randomRs ('A','Z') g
      vals = mkv 140 $ randomRs ('a','z') g
  res <- mapM (uncurry (MC.set mc)) (take 10000 $ zip keys vals)
  if any not res
     then putStrLn "Something wrong has happened."
     else putStrLn "Successfully inserted random values."