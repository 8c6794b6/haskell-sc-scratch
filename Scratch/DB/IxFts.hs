{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-|

Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Simple adhoc ixset searching web ui with indexer. Document database
contains word chunks of given file, separating the contents with
space.  May not so much useful with documents that not using space as
separator.

Since the server is loading the whole set of indice, the first query
after running the server will be slow. From the second query, it will
be better.

When index command has specified with '.html' suffix, the body
contents of html document would be stored in database for searching.

-}
module IxFts where

import Prelude hiding ((.),id,div,head)

import Control.Applicative (Applicative(..), (<$>))
import Control.Category
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Data (Data,Typeable)
import Data.List hiding (head,insert)
import Data.Function (on)
import Data.Map (Map)

import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Data.Acid
import Data.IxSet
import Data.SafeCopy
import Happstack.Server hiding (body, method)
import System.Console.CmdArgs hiding (name)
import System.Directory.Tree (readDirectoryWith, AnchoredDirTree(..))
import Text.Blaze.Html5 hiding (base,map)
import Text.Blaze.Html5.Attributes hiding
  (dir,id,title,form,style,span,size)
import Text.HTML.TagSoup (Tag(..), (~==), innerText, parseTags, sections)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Text.Blaze.Html5.Attributes as A

------------------------------------------------------------------------------
-- Database

data Document = Document
  { docPath :: DocPath
  , docWordCount :: Int
  , docWordMap :: Map ByteString Int
  , docContents :: Contents
  } deriving (Eq,Ord,Data,Typeable)

instance Show Document where
  show (Document (DocPath p) _ _ _) = "Document " ++ p

newtype DocPath = DocPath {unDocPath::FilePath}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Contents = Contents {unContents::ByteString}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype Word = Word {unWord::ByteString}
  deriving (Eq,Ord,Show,Data,Typeable)

newtype DocDB = DocDB {docIx :: IxSet Document}
  deriving (Eq,Show,Data,Typeable)

instance Indexable Document where
  empty = ixSet
    [ixFun (return . docPath)
    ,ixFun (map Word . mkChunks . unContents . docContents)]

mkChunks :: ByteString -> [ByteString]
mkChunks = C8.splitWith (`elem` " \t\n.,!?&()[]{}<>;/\"'")

$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocPath)
$(deriveSafeCopy 0 'base ''Contents)
$(deriveSafeCopy 0 'base ''Word)
$(deriveSafeCopy 0 'base ''DocDB)

saveDoc :: IxSet Document -> Update DocDB ()
saveDoc ixs = put (DocDB ixs)

loadDoc :: Query DocDB DocDB
loadDoc = ask

$(makeAcidic ''DocDB ['saveDoc, 'loadDoc])

getDB :: IO DocDB
getDB = do
  db <- openAcidState (DocDB empty)
  query db LoadDoc

------------------------------------------------------------------------------
-- Indexer

index :: FilePath -> Maybe String -> IO ()
index root ext = do
  db <- openAcidState (DocDB empty)
  let mkIx = case ext of
        Just e | "html" `isSuffixOf` e -> mkHtmlDocument
               | otherwise             -> mkDocument (e `isSuffixOf`) C8.pack
        Nothing -> mkDocument (const True) C8.pack
  ixs <- mkIx root
  update db (SaveDoc ixs)
  putStrLn "Done."

mkDocument :: (FilePath -> Bool) -> (String -> ByteString)
           -> FilePath -> IO (IxSet Document)
mkDocument p f root = do
  _ :/ docs <- flip readDirectoryWith root $ \path -> do
    if p path then do
      putStrLn $ "Including: " ++ path
      bs <- f <$> readFile path
      let dpth = DocPath path
          dcon = Contents bs
          wds = mkChunks bs
          dmap = foldr (\w m -> M.insertWith (+) w 1 m) M.empty wds
      return $ Just $ Document dpth (length wds) dmap dcon
    else
      return Nothing
  putStrLn "Working .... "
  return $ F.foldr (maybe id (insert)) empty docs

mkHtmlDocument :: FilePath -> IO (IxSet Document)
mkHtmlDocument = mkDocument (".html" `isSuffixOf`) $ \cs ->
  C8.pack . innerText . join . sections (~== (TagOpen "body" [])) .
  parseTags . map toLower $ cs

------------------------------------------------------------------------------
-- Server

serve :: Int -> DocDB -> IO ()
serve p db = do
  putStrLn $ "Starting server with port: " ++ show p
  simpleHTTP nullConf {port=p} $ msum
    [ dir "favicon.ico" $ notFound $ toResponse ()
    , dir "static" $ serveDirectory EnableBrowsing [] "./static"
    , searchPage db
    , seeOther "" $ toResponse () ]

searchPage :: DocDB -> ServerPartT IO Response
searchPage db = do
  qs <- getDataFn $ look "q"
  case qs of
    Left _ -> do
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml "ixset search"
          css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            inputForm ""
    Right qs' -> do
      let res = docIx db @* (map Word . C8.words . C8.pack $ qs')
      ok $ toResponse $ html $ do
        head $ do
          title $ toHtml $ "search result for " ++ qs'
          css
        body $ do
          div ! class_ (toValue "wrapper") $ do
            inputForm qs'
            div ! class_ (toValue "summary") $ toHtml $
              "search result for: \"" ++ qs' ++ "\", " ++
              "hit: " ++ show (size res) ++ ""
            ul $ mapM_ mkLink (sortBy (compare `on` score qs') $ toList res)

score :: String -> Document -> Double
score ws doc = foldr f 0 (C8.words $ C8.pack ws) where
  f w acc = acc + (total / fromIntegral (M.findWithDefault 0 w dmap))
  dmap = docWordMap doc
  total = fromIntegral $ docWordCount doc

mkLink :: Document -> Html
mkLink doc = do
  let path = unDocPath $ docPath doc
  li $ a ! href (toValue path) $ do
    toHtml $ dropWhile (/= '/') path

inputForm :: String -> Html
inputForm val =
  div ! class_ (toValue "input_form") $ do
    form !
      enctype (toValue "multipart/form-data") !
      method (toValue "GET") !
      action (toValue "/") $ do
        input !
          type_ (toValue "text") !
          name (toValue "q") !
          A.size (toValue "40") !
          value (toValue val) !
          autofocus (toValue "autofocus")
        input !
          type_ (toValue "submit") !
          value (toValue "search")

css :: Html
css = style ! type_ (toValue "text/css") $ toHtml
 "body { font-size: 15px; } \
\input { margin: 5px; border: 1px solid #868686; }\
\div.wrapper { padding: 20px; } \
\div.wrapper ul { list-style: none; padding-left: 10px; } \
\div.wrapper ul li { margin: 5px 0 } \
\div.wrapper ul li a { text-decoration: none; } \
\div.summary { font-size: 75%; padding-left: 20px; }"

------------------------------------------------------------------------------
-- CLI

data IxFts
  = Index {docs :: FilePath, ext :: Maybe String}
  | Serve {portNum :: Int}
    deriving (Eq,Show,Data,Typeable)

main :: IO ()
main = do
  args <- cmdArgs $ modes
    [ Index { docs = def &= typ "PATH" &=
                     help "Path to directory containing target documents"
            , ext = def &= typ "EXTENSION" &=
                    help "File extension to read" } &=
      help "Index documentas under given path"
    , Serve { portNum = 8000 &= typ "PORT" &= help "Port number to serve"} &=
      help "Serve database with web ui" ]
  case args of
    Index path e -> index path e
    Serve p      -> do
      db <- getDB
      db `seq` serve p $! db
