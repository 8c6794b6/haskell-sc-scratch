{-| 

Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

-}
module HelloWx where

import Control.Concurrent
import Graphics.UI.WX
import Graphics.UI.WX.Layout

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  f <- frame [text := "Hello World!"]
  t <- staticText f [text := "Hello StaticText!"]
  set f [text :~ (++ " and hello again!")]
  b <- button f [text := "Hello Button!"
                ,on command := set t [text:="You have clicked the button!"]]
  q <- button f [text := "Quit!"
                ,on command := close f]
  c <- checkBox f [text := "Hello CheckBox!"]
  r <- radioBox f Vertical ["Hello Option 1", "Hello Option 2"] []
  hs <- hslider f True 0 100 [text := "HSlider"]
  vs <- vslider f True 0 100 [text := "VSlider"]
  set f [layout :=
         boxed "Hello Box 1" $
         column 0 [row 0 [widget c
                         ,boxed "Hello Box 2" $
                          column 0 [widget t, widget b, widget q]]
                  ,boxed "Hello sliders " $ 
                   row 0 [widget hs, widget vs]
                  ,boxed "Hello Radioo" $ widget r]]
    
modify :: w -> Attr w a -> (a -> a) -> IO ()
modify w attr f = do
  val <- get w attr
  set w [attr := f val]