module Main where

import Control.Applicative
import Control.Monad 
    ( forM
    , filterM
    , join
    , when )
import Data.List (isSuffixOf)
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.String.Utils (replace)
import qualified Database.TokyoCabinet as TC
import qualified Database.TokyoDystopia as TD
import Text.HTML.TagSoup
import Text.Regex (subRegex, mkRegex)

main :: IO ()
main = do 
  target <- getArgs
  let target' = case target of
                 [] -> "target"
                 (a:_) -> a
  ts <- mkTargets target' 
  i <- mkIndice ts 
  d <- mkData ts
  print [("index creation:", i), 
         ("data insertion:", d)]

-- | Datatype to hold information of target.
data Target = Target {
      targetId :: Int,
      targetUrl :: String,
      targetText :: String
    } deriving (Eq, Show, Read)

-- | XXX: Get this string from command line, and pass to ReaderT IO or whatever.
documentRoot :: String
documentRoot = "localhost:8000/"

-- | Makes index for tokyodystipia from Target.
mkIndice :: [Target] -> IO Bool
mkIndice ts = do
  db <- TD.tcidbnew
  TD.open db "casket" [TD.OCREAT, TD.OWRITER]
  res <- fmap and $ sequence $ 
         zipWith (TD.put db) (map (fromIntegral . targetId) ts) (map targetText ts)
  TD.close db
  return res

-- | Inserts targets to tokyocabinet hashed format database.
mkData :: [Target] -> IO Bool
mkData ts = do
  res <- TC.runTCM $ do
       db <- TC.new :: TC.TCM TC.HDB
       TC.open db "db.tch" [TC.OCREAT, TC.OWRITER]
       let task db t = do
            TC.put db ("url:" ++ (show $ targetId t)) (targetUrl t)
            TC.put db ("text:" ++ (show $ targetId t)) (targetText t)
       res <- sequence $ map (task db) ts
       TC.close db
       return res
  return $ and res

-- | Makes Target from given filepath.
mkTargets :: FilePath -> IO [Target]
mkTargets root = do
  htmls <- filter ("html" `isSuffixOf`) <$> recurseDirectory root
  let urls = map (\x -> "http://" </> documentRoot </> x) htmls
  bodies <- unsafeInterleaveMapIO (fmap bodyText . readFile) htmls
  return . getZipList $ 
         Target <$> ZipList [1..] <*> ZipList urls <*> ZipList bodies

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

