------------------------------------------------------------------------------
-- | Playing with threads in haskell.
--
module ThreadPlay where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import System.Random

master01 :: IO ()
master01 = do
  mvar <- newMVar ()
  gen <- newStdGen
  let childIds = take 10 $ randomRs (0,maxBound) gen 
      childDelays = randomRs (10,1000) gen
  sequence_ $ getZipList $ goChild mvar <$> ZipList childIds <*> ZipList childDelays

goChild :: MVar () -> Int -> Int -> IO ThreadId
goChild mv cId del = forkIO $ do
  takeMVar mv
  putStrLn $ "ID: " ++ show cId ++ " del: " ++ show del
  threadDelay (del * 10 ^ 3)
  putStrLn $ "done ID: " ++ show cId
  putMVar mv ()

type ThreadBank = (Int,Map Int (ThreadId,MVar ()))

newThreadBank :: IO (MVar ThreadBank)
newThreadBank = newMVar (0,M.empty)

addThread :: MVar ThreadBank -> ChildThread () -> IO ThreadBank
addThread mvar act = do
  (currentId,bank) <- takeMVar mvar
  childVar <- newMVar ()
  tid <- forkIO $ runReaderT act childVar
  let currentId' = currentId + 1
      bank' = M.insert currentId' (tid,childVar) bank
  putMVar mvar (currentId',bank')
  return (currentId',bank')

deleteThread  :: MVar ThreadBank -> Int -> IO ThreadBank
deleteThread mvar key = do
  b@(currentId,bank) <- takeMVar mvar
  case M.lookup key bank of
    Just (tid,_) -> do 
      killThread tid
      let retval = (currentId, M.delete key bank)
      putMVar mvar retval >> return retval
    Nothing -> do
      putMVar mvar b >> return b

lookupThreadId :: MVar ThreadBank -> Int -> IO (Maybe ThreadId)
lookupThreadId mvar key = do
  (_,bank) <- readMVar mvar
  return . (fmap fst) $ M.lookup key bank

showThreadBank :: MVar ThreadBank -> IO ()
showThreadBank mvar = do
  (_,bank) <-  readMVar mvar
  print $ M.map fst bank -- map fst $ M.elems bank

tDelay :: Int -> ChildThread ()
tDelay time = do
  mv <- ask 
  lift $ do {takeMVar mv; threadDelay time; putMVar mv ()}

type ChildThread a = ReaderT (MVar ()) IO a

runChild :: MVar () -> ChildThread a -> IO a
runChild mv a = runReaderT a mv

thread01 :: ChildThread ()
thread01 = do
  lift $ putStrLn "hello"
  tDelay (2 * 10 ^ 6)
  lift $ putStrLn "world"
                
-- | A function that takes 3 args.
zipme :: Int -> Int -> Int -> Int
zipme a b c = a + b + c

-- | Zipping with @zipme@.
zipWithZipme :: [Int]
zipWithZipme = getZipList $ zipme <$> 
               ZipList [1..] <*> ZipList [11..] <*> ZipList [101..111]

-- | An io action that takes 3 args.
zipmeM :: Int -> Int -> Int -> IO Int
zipmeM a b c = return $ a + b + c

-- | Zipping with @zipmeM@.
zipWithZipmeM :: IO [Int]
zipWithZipmeM = sequence $ getZipList $ 
                zipmeM <$> z [1..] <*> z [11..] <*> z [101..111] 
                    where z = ZipList

goEcho :: IO ThreadId
goEcho = do
  var <- newEmptyMVar
  forkIO $ echo var

echo :: MVar () -> IO ()
echo c = do
  msg <- takeMVar c
  print msg

type IntFunc = Int -> Int -> IO Int

goFunc = do
  var <- newMVar (\a b -> return (a + b))
  forkIO $ func var

func :: MVar IntFunc -> IO ()
func c = do
  f <- takeMVar c
  result <- f 1 2 
  print result

goPrimeAndFib :: IO ()
goPrimeAndFib = do
  ps <- run primes
  fs <- run fibonacci
  mapM_ print $ zip ps fs
 where
   run f = do
     c <- newChan
     l <- getChanContents c
     forkIO $ writeList2Chan c f
     return l 

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p:sieve [x | x <- xs, x `mod` p > 0]

fibonacci :: [Integer]
fibonacci = 0:1:zipWith (+) fibonacci (tail fibonacci)
   
    
        