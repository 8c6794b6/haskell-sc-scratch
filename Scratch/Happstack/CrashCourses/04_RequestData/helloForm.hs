{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/RqData.html>

-}
module Main where

import Control.Applicative
import Control.Monad (msum)

import Happstack.Server
  (Response, ServerPart, Method(..), BodyPolicy(..)
  ,badRequest, body, decodeBody, defaultBodyPolicy, dir, nullConf, ok
  ,queryString, simpleHTTP, toResponse, methodM)
import Happstack.Server.RqData 
  (RqData, checkRq, look, lookFile, lookRead, getDataFn)
import Text.Blaze.Html5 ((!), html, title, form)  
  
import qualified Text.Blaze as B hiding (string)
import qualified Text.Blaze.Html5 as B hiding (map, string)
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = simpleHTTP nullConf handlers

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" (10*10^6) 1000 1000

handlers :: ServerPart Response
handlers = do
  decodeBody myPolicy
  msum [ dir "helloForm" helloForm
       , dir "hello" helloPart
       , dir "hello2" helloPart2
       , dir "hello3" helloPart3
       , dir "read" intPart
       , dir "check" votePart
       , dir "onetoten" oneToTenPart
       , upload
       , listingPart ]

listingPart :: ServerPart Response
listingPart = ok $ toResponse $
  B.html $ do
    B.head $ do
      B.title "Forms and decodebody examples"
    B.body $ do
      B.h1 $ B.toHtml ("Request data and forms" :: String)
      B.ul $ do
        B.li $ B.a ! A.href "/helloForm" $ B.toHtml ("helloForm" :: String)
        B.li $ B.a ! A.href "/hello2" $ B.toHtml ("hello2" :: String )
        B.li $ B.a ! A.href "/hello3" $ B.toHtml ("hello3" :: String )
        B.li $ B.a ! A.href "/upload" $ B.toHtml ("upload" :: String )
        B.li $ B.a ! A.href "/read?int=100" $ 
          B.toHtml ("read int from querystring" :: String )
        B.li $ B.a ! A.href "/check?vote=yay" $ B.toHtml ("vote with checkRq" :: String )
        B.li $ B.a ! A.href "/onetoten?i=8" $ B.toHtml ("check in range" :: String )

helloForm :: ServerPart Response
helloForm = ok $ toResponse $ 
  B.html $ do
    B.head $ do
      B.title "Hello Form"
    B.body $ do
      B.form ! A.enctype "multipart/form-data" ! 
        A.method "POST" ! A.action "/hello" $ do
          B.label "greeting: " 
          B.input ! A.type_ "text" ! A.name "greeting" ! A.size "10"
          B.label "noun:     "
          B.input ! A.type_ "text" ! A.name "noun" ! A.size "10"
          B.input ! A.type_ "submit" ! A.value " go "

helloPart :: ServerPart Response
helloPart = do
  greeting <- body $ look "greeting"
  noun <- body $ look "noun"
  ok $ toResponse $ greeting ++ ", " ++ noun ++ "\n"
  
helloRq2 :: RqData (String, String)
helloRq2 = (,) <$> look "greeting" <*> look "noun"

helloPart2 :: ServerPart Response
helloPart2 = do
  r <- getDataFn helloRq2
  case r of
    Left e -> badRequest $ toResponse $ unlines e
    Right (greet,noun) -> ok $ toResponse $ greet ++ ", " ++ noun

helloPart3 :: ServerPart Response
helloPart3 = do 
  greet <- optional $ look "greeting"
  ok $ toResponse $ show greet
  
upload :: ServerPart Response
upload =
  msum [ dir "post" post
       , dir "upload" uploadForm ]

uploadForm :: ServerPart Response
uploadForm = ok $ toResponse $
  html $ do
    B.head $ do
      title "Upload Form"
    B.body $ do
      B.form ! A.enctype "multipart/form-data" ! A.method "POST" ! A.action "/post" $ do
        B.input ! A.type_ "file" ! A.name "file_upload" ! A.size "40"
        B.input ! A.type_ "submit" ! A.name "upload"

post :: ServerPart Response
post = do
  r <- lookFile "file_upload"
  ok $ toResponse $
    B.html $ do
      B.head $ B.title "Post Data"
      B.body $ mkBody r
  where
    mkBody (tmpFile, uploadName, contentType) = do
      B.p $ B.toHtml $ "temporary file: " ++ tmpFile
      B.p $ B.toHtml $ "uploaded name:  " ++ uploadName
      B.p $ B.toHtml $ "content-type:   " ++ show contentType
      
lookInt :: RqData Int
lookInt = lookRead "int"

intPart :: ServerPart Response
intPart = do
  r <- getDataFn lookInt
  case r of 
    Left e -> badRequest $ toResponse $ unlines e
    Right i -> ok $ toResponse $ "Read the int: " ++ show i
    
data Vote = Yay | Nay     
  deriving (Eq, Ord, Read, Show, Enum, Bounded)
           
parseVote :: String -> Either String Vote           
parseVote "yay" = Right Yay
parseVote "nay" = Right Nay
parseVote str   = Left $ "Expecting 'yay' or 'nay' but got: " ++ str

votePart :: ServerPart Response
votePart = do
  r <- getDataFn $ look "vote" `checkRq` parseVote
  case r of
    Left e  -> badRequest $ toResponse $ unlines e
    Right i -> ok $ toResponse $ "You voted: " ++ show i

inRange :: (Show a, Ord a) => a -> a -> a -> Either String a
inRange lower upper a
  | lower <= a && a <= upper = Right a
  | otherwise = Left $ show a ++ " is not between " ++ show lower ++ 
                " and " ++ show upper
                
oneToTenPart :: ServerPart Response                
oneToTenPart = do
  r <- getDataFn (lookRead "i" `checkRq` inRange 1 10)
  case r of
    Left e  -> badRequest $ toResponse $ unlines e
    Right i -> ok $ toResponse $ "You picked: " ++ show i
