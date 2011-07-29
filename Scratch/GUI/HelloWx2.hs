{-| 

Module      : $Header$
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : portable

Second sample app with wx. 

Main points in this code is:

* To layout sliders and buttons, with adjusting size and values.

* DONE: To get values from sliders and pass to IO action.

* To show multiple windows.

-}
module HelloWx2 where

import Control.Monad

import Graphics.UI.WX
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Types

main :: IO ()
main = start gui

-- | Guts of gui action.
gui :: IO ()
gui = do
  f <- frame [text := "Hello wx 2nd"]
  p <- panel f [text := "Hello Panel"]
  q <- button p [text := "x", on command := close f]
  b <- button p [text := "Fuff!", on command := putStrLn "Fuff!"]
  vs <- mapM (mkVS p) [1..12]
  set f [layout := widget p]
  set p [size := sz 250 250
        ,layout := boxed "2nd" $ 
         column 0 [row 0 [widget q, widget b]
                  ,row 0 [boxed "sliders" $ row 0 vs]]]
    
mkVS frm n = do
  let name = "slider" ++ show n
  sldr <- sliderEx frm 0 100 vslInversedStyle [text := name]
  set sldr [on command := get sldr selection >>= \value ->
             putStrLn (name ++ ":" ++ show value)]
  return $ boxed (show n) $ widget sldr
    
-- | Style for inversed vertical slider.
vslInversedStyle :: Style
vslInversedStyle = 
  --XXX: Include wx header file so that each style could referred by name.
  0x0008 + -- vertical 
  0x0010 + -- tick
  0x0020 + -- labels
  0x0080 + -- top
  0x1000   -- inverse
