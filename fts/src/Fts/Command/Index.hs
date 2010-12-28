module Fts.Command.Index where

import Control.Applicative (ZipList(..), (<*>), (<$>))
import Control.Monad (join, when)
import Data.List (isSuffixOf)
import System.Directory (createDirectory, doesDirectoryExist)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as C8

import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, sections)
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia as TD
import qualified System.Directory.Tree as T

import qualified Fts.Model as M

-- | Index html file under specified directory and make indexed data.
run :: FilePath -- ^ Database path
    -> FilePath -- ^ Target path
    -> IO ()
run dbPath targetPath = do
  putStrLn $ "Output: " ++ dbPath
  putStrLn $ "Target: " ++ targetPath
  mkOutDirectories dbPath
  ts <- mkTargets targetPath
  putStrLn "Creating key-value database ..."
  d <- mkData dbPath ts
  putStrLn "Creating index ... "
  i <- mkIndice dbPath ts
  let message | i && d      = "Done"
              | not i && d  = "Index creation failed"
              | i && not d  = "Key value database creation failed"
              | otherwise   = "Index and key value database creation failed"
  putStrLn message

-- | Datatype to hold information of target.
data Target = Target {
      targetId :: Int,
      targetUrl :: String,
      targetText :: String
    } deriving (Eq, Show, Read)

-- | Make output director if not exist
mkOutDirectories :: FilePath -> IO ()
mkOutDirectories dbPath = mapM_ go [dbPath, M.tdDBPath dbPath]
  where
    go path = do
      exist <- doesDirectoryExist path
      when (not exist) (createDirectory path)

-- | Makes index for tokyodystipia from Target.
mkIndice :: FilePath -> [Target] -> IO Bool
mkIndice dbPath ts = TD.runTDM $ do
  db <- TD.new :: TD.TDM TD.IDB
  TD.open db (M.tdDBPath dbPath) [TD.OCREAT, TD.OWRITER]
  res <- fmap and $ sequence $
         zipWith (TD.put db) (map (fromIntegral . targetId) ts)
                     (map (C8.pack . targetText) ts)
  TD.close db
  return res

-- | Inserts targets to tokyocabinet hashed format database.
mkData :: FilePath -> [Target] -> IO Bool
mkData dbPath ts = do
  res <- TC.runTCM $ do
       db <- TC.new :: TC.TCM TC.HDB
       TC.open db (M.tcDBPath dbPath) [TC.OCREAT, TC.OWRITER]
       let task db' t = do
            TC.put db' ("url:" ++ (show $ targetId t)) (targetUrl t)
            TC.put db' ("text:" ++ (show $ targetId t)) (targetText t)
       res <- sequence $ map (task db) ts
       TC.close db
       return res
  return $ and res

-- | Make Target from given filepath.
mkTargets :: FilePath -> IO [Target]
mkTargets root = do
  htmls <- filter ("html" `isSuffixOf`) <$> recurseDirectory root
  bodies <- unsafeInterleaveMapIO (fmap bodyText . readFile) htmls
  return . getZipList $
    Target <$> ZipList [1..] <*> ZipList htmls <*> ZipList bodies

-- | Read html strings and extract body text.
bodyText :: String -> String
bodyText html = parse html
  where
    parse = innerText . join . sections (~== (TagOpen "body" [])) . parseTags

-- | Recurse under given directory and return list of filepath.
recurseDirectory :: FilePath -> IO [FilePath]
recurseDirectory root =
  T.readDirectoryWithL return root >>= return . F.toList . T.free

-- | From:
--
-- http://www.haskell.org/pipermail/haskell-cafe/2006-October/019064.html
--
unsafeInterleaveMapIO :: (a -> IO b) -> [a] -> IO [b]
unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
  y <- f x
  ys <- unsafeInterleaveMapIO f xs
  return (y:ys)
unsafeInterleaveMapIO _ [] = return []
