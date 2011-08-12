{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -fno-warn-orphans #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Example from:

<http://happstack.com/docs/crashcourse/Templates.html>

-}
module Main where

import Control.Applicative ((<$>), optional)
import Control.Monad (msum)
import Data.Maybe (fromMaybe)
import System.Random (Random(..))

import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (liftIO)
import Happstack.Server
  (Response, ServerPartT, dir, mapServerPartT, look, nullConf
  ,ok, simpleHTTP, toResponse)
import Happstack.Server.HSP.HTML (defaultTemplate)
import Happstack.Server.JMacro ()
import HSP
  (Attr(..), EmbedAsAttr(..), EmbedAsChild(..), genElement, genEElement)
import HSX.JMacro (IntegerSupply(..), nextInteger')
import Language.Javascript.JMacro
  (ToJExpr(..), Ident(..), JStat(..), JExpr(..), JVal(..)
  ,jmacro, jsv, jLam, jVarTy)

import qualified Data.Map as Map

type JMacroPart = ServerPartT (StateT Integer IO)

instance IntegerSupply JMacroPart where
  nextInteger = nextInteger'

main :: IO ()
main = simpleHTTP nullConf $ mapServerPartT (flip evalStateT 0) handlers

handlers :: JMacroPart Response
handlers = msum
  [dir "hello" $ helloJMacro
  ,dir "attr" $ helloAttr
  ,dir "endtag" $ helloEndTag
  ,dir "clickme" $ clickPart
  ,dir "clickme2" $ clickPart2
  ,dir "functions" $ functionNames
  ,dir "fortune" $ fortunePart
  ,dir "weather" $ weatherPart
  ,externalPart
  ,demosPart]

demosPart :: JMacroPart Response
demosPart =
  toResponse <$> defaultTemplate "demos" ()
    <ul>
      <li><a href="/hello">Hello, JMacro</a></li>
      <li><a href="/attr">Hello, Attr</a></li>
      <li><a href="/endtag">Hello, End Tag</a></li>
      <li><a href="/clickme">ClickMe</a></li>
      <li><a href="/clickme2">ClickMe2</a></li>
      <li><a href="/functions">Function Names</a></li>
      <li><a href="/fortune">Fortune</a></li>
      <li><a href="/weather">Weather</a></li>
      <li><a href="/external">External</a></li>
    </ul>

helloJMacro :: JMacroPart Response
helloJMacro =
  toResponse <$> defaultTemplate "Hello JMacro" ()
  <div>
    <% [$jmacro|
        var helloNode = document.createElement('h1');
        helloNode.appendChild(document.createTextNode("Hello, JMacro!"));
        document.body.appendChild(helloNode);
        |] %>
  </div>

helloAttr :: JMacroPart Response
helloAttr =
  toResponse <$> defaultTemplate "Hello Attr" ()
  <h1 style="cursor:pointer"
      onclick=[$jmacro| alert("that </tickles>!") |]>Click me!</h1>

helloEndTag :: JMacroPart Response
helloEndTag =
  toResponse <$> defaultTemplate "Hello End Tag" ()
  <%>
    <h1>Tricky End Tag</h1>
    <% [$jmacro| alert("this </script> won't mess things up!") |] %>
  </%>

clickMe :: JStat
clickMe =
  [$jmacro|
   var clickNode = document.createElement('p');
   clickNode.appendChild(document.createTextNode("Click me!"));
   document.body.appendChild(clickNode);
   var clickCnt = 0;
   clickNode.setAttribute('style', 'cursor: pointer');
   clickNode.onclick = function () {
     clickCnt++;
     alert('Been clicked ' + clickCnt + ' time(s).');
   };
  |]

clickPart :: JMacroPart Response
clickPart =
  toResponse <$> defaultTemplate "Hygienic Naming" ()
  <div>
    <h1>A Demo of Happstack + HSP + JMacro </h1>
    <% clickMe %>
    <% clickMe %>
  </div>

clickMe2Init :: JStat
clickMe2Init = [$jmacro| var !clickCnt = 0; |];

clickMe2 :: JStat
clickMe2 =
  [$jmacro|
   var clickNode = document.createElement('p');
   clickNode.appendChild(document.createTextNode("Click me!"));
   document.body.appendChild(clickNode);
   clickNode.setAttribute("style", "cursor: pointer");
   clickNode.onclick = function () {
     clickCnt++;
     alert('Been clicked ' + clickCnt + ' time(s).');
   };
  |]

clickPart2 :: JMacroPart Response
clickPart2 =
  toResponse <$> defaultTemplate "Hygienic Naming"
  <% clickMe2Init %>
  <div>
    <h1>A Demo of Happstack + HSP + JMacro </h1>
    <% clickMe2 %>
    <% clickMe2 %>
  </div>

functionNames :: JMacroPart Response
functionNames =
  toResponse <$> defaultTemplate "Function Names"
  <%
  [$jmacro|
   function !hello(noun) {alert('hello, ' + noun);}
   var !helloAgain = \noun -> alert('hello again, ' + noun);
   fun goodbye noun {alert('goodbye ' + noun);}
   fun goodbyeAgain noun -> alert('goodbye again, ' + noun);
  |]
  %>
  <%>
    <button onclick=[$jmacro| hello('world'); |]>hello</button>
    <button onclick=[$jmacro| helloAgain('world'); |]>helloAgain</button>
    <button onclick=[$jmacro| goodbye('world'); |]>goodbye</button>
    <button onclick=[$jmacro| goodbyeAgain('world'); |]>goodbyeAgain</button>
  </%>

fortunePart :: JMacroPart Response
fortunePart = do
  let fortunes = ["You will be cursed to write Java for the rest of your days."
                 ,"Fortune smiles upon you, your fortune will be filled with lambdas."]
  n <- liftIO $ randomRIO (0, length fortunes - 1)
  toResponse <$> defaultTemplate "Fortune"
    <% [$jmacro|
        fun revealFortune fortune {
          var b = document.getElementById("button");
          b.setAttribute('disabled', 'disabled');
          var p = document.getElementById("fortune");
          p.appendChild(document.createTextNode(fortune));
        }
     |] %>
    <div>
      <h1>Your Fortune</h1>
      <p id="fortune"></p>
      <button id="button" onclick=[$jmacro| revealFortune(`(fortunes !! n)`); |]>
        Click to reveal your fortune
      </button>
    </div>

data Skies = Cloudy | Clear
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

newtype Fahrenheit = Fahrenheit Double
  deriving (Num, Enum, Eq, Ord, Read, Show, ToJExpr, Random)

data Weather = Weather
  { skies :: Skies
  , temp  :: Fahrenheit
  } deriving (Eq, Ord, Read, Show)

instance Random Skies where
  randomR (lo, hi) g = (toEnum c,g') where
    (c, g') = randomR (fromEnum lo, fromEnum hi) g
  random g = randomR (minBound, maxBound) g

instance Random Weather where
  randomR (Weather slo flo, Weather shi fhi) g0 = (Weather s f, g2) where
    (s, g1) = randomR (slo, shi) g0
    (f, g2) = randomR (flo, fhi) g1
  random g = (Weather s f, g'') where
    (s,g') = random g
    (f,g'') = random g'

instance ToJExpr Skies where
  toJExpr = toJExpr . show

instance ToJExpr Weather where
  toJExpr (Weather skies temp) =
    toJExpr $ Map.fromList [("skies", toJExpr skies),("temp", toJExpr temp)]

weatherPart :: JMacroPart Response
weatherPart = do
  weather <- liftIO $ randomRIO (Weather minBound (-40), Weather maxBound 100)
  toResponse <$> defaultTemplate "Weather Report" ()
    <div>
      <% [$jmacro|
          var w = `(weather)`;
          var p = document.createElement('p');
          p.appendChild(document.createTextNode(
            "The skies will be " + w.skies +
            " and the temperature will be " + w.temp.toFixed(1) + "F"));
          document.body.appendChild(p);
       |] %>
    </div>

externalJs :: String -> JStat
externalJs greeting =
  [$jmacro|
   fun greet noun {
     alert(`(greeting)` + ' ' + noun);
   }
  |]

externalPart :: JMacroPart Response
externalPart = dir "external" $ msum
  [ dir "script.js" $ do
      greeting <- optional $ look "greeting"
      ok $ toResponse $ externalJs (fromMaybe "hello" greeting)
  , toResponse <$> defaultTemplate "external"
    <script type="text/javascript" src="/external/script.js?greeting=Ahoy" />
    <div>
      <h1>Greeting</h1>
      <button onclick=[$jmacro| greet('JMacro'); |]>Click for a greeting.</button>
    </div>
   ]
