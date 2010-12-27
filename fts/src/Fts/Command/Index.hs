module Fts.Command.Index where

import Control.Applicative
import Control.Monad
    ( forM
    , join )
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Char8 as C8

import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia as TD
import Text.HTML.TagSoup
import Text.Regex (subRegex, mkRegex)

import qualified Fts.Model as M

-- | Index html file under specified directory and make indexed data.
run :: FilePath -- ^ Database path
    -> FilePath -- ^ Template path
    -> IO ()
run dbPath templatePath= do
  ts <- mkTargets templatePath
  i <- mkIndice dbPath ts
  d <- mkData dbPath ts
  print [("index creation:", i),
         ("data insertion:", d)]

-- | Datatype to hold information of target.
data Target = Target {
      targetId :: Int,
      targetUrl :: String,
      targetText :: String
    } deriving (Eq, Show, Read)

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

-- | Makes Target from given filepath.
mkTargets :: FilePath -> IO [Target]
mkTargets root = do
  htmls <- filter ("html" `isSuffixOf`) <$> recurseDirectory root
  bodies <- unsafeInterleaveMapIO (fmap bodyText . readFile) htmls
  return . getZipList $
         Target <$> ZipList [1..] <*> ZipList htmls <*> ZipList bodies

-- | Read html strings and extract body text.
bodyText :: String -> String
bodyText html = tidy (parse html)
    where
      parse = innerText . join . sections (~== (TagOpen "body" [])) . parseTags
      tidy a = subRegex (mkRegex "\\\n+") a "\n"

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

-- | From RWH's I/O Case Study chapter.
recurseDirectory :: FilePath -> IO [FilePath]
recurseDirectory root = do
  paths <- getDirectoryContents root
  let paths' = filter (`notElem` [".", ".."]) paths
  paths'' <- forM paths' $ \name -> do
    let path = root </> name
    directoryExists <- doesDirectoryExist path
    if directoryExists
      then recurseDirectory path
      else return [path]
  return $ concat paths''
