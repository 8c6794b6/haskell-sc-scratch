{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Using text file to make OSC message for input of scsynth non-realtime
sound synthesis.

-}
module PT where

import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

main :: IO ()
main = do
  mapM_ (uncurry writeSynthdef)
    [("pt01",pt01),("pt02",pt02),("pt03",pt03),("pt04",pt04)]
  (infile:outfile:_) <- getArgs
  writeNRT outfile =<< go infile

pt01 :: UGen
pt01 =
  let sig = sinOsc AR frq 0 * amp * e
      amp = "amp"@@0.3
      frq = "freq"@@440
      e = envGen KR 1 1 0 1 RemoveSynth $ envPerc 1e-3 3e-1
  in  out 0 (mce [sig,sig])

pt02 :: UGen
pt02 =
  let sig = sinOsc AR frq 0 * amp
      frq = "freq"@@440
      amp = envGen KR 1 ("amp"@@0.02) 0 dur RemoveSynth $ envPerc 1e-3 1
      dur = "dur"@@0.1
  in  out 0 $ mce [sig,sig]

pt03 :: UGen
pt03 =
  let sig = sinOsc AR frq 0 * amp
      frq = ffr + mod
      ffr = "freq"@@440
      mod = sinOsc KR (ffr*5) 0 * (idx*ffr)
      idx = "idx"@@1
      amp = envGen KR 1 ("amp"@@0.020) 0 dur RemoveSynth $ envPerc 1e-3 1
      dur = 0.2
      pan = "pan"@@0
  in  out 0 $ pan2 sig pan 1

pt04 :: UGen
pt04 =
  let sig = ringz (whiteNoise 'Γ' AR) frq ("q"@@0.1) * amp
      amp = envGen KR 1 ("amp"@@0.008) 0 dur RemoveSynth $ envPerc 1e-4 1
      frq = "freq"@@440
      dur = "dur"@@1
  in  out 0 $ pan2 sig ("pan"@@0) 1

-- mkOSC :: Int -> Char -> OSC
-- mkOSC i c =
--   s_new "pt01" (-1) AddToTail 1
--   [("freq",fromIntegral i*100),("amp",(fromIntegral $ fromEnum c) / 128)]

-- | Using pt01, Char code used for amplitude.
mkOSC1 :: String -> [OSC]
mkOSC1 ls = zipWith g [0,1..] ls where
  g i c =
    s_new "pt01" (-1) AddToTail 1
    [("freq",fromIntegral i * 100),("amp",(fromIntegral $ fromEnum c) / 128)]

-- | Using pt02, Char code used for grain duration, linear frequency.
mkOSC :: Int -> Char -> [OSC]
mkOSC i c =
  [s_new "pt02" (-1) AddToTail 1
   [("freq",100 + (1.1**fromIntegral i)),("dur",c')]]
  where
    c' = (fromIntegral (fromEnum c - 32) / 94)

-- | Using pt02. Char code used for grain duration, exponential frequency.
mkOSC' :: String -> [OSC]
mkOSC' ls = catMaybes $ zipWith g [0,1..] ls where
  g i c | c == ' ' = Nothing
        | otherwise = Just $ s_new "pt02" (-1) AddToTail 1
                      [("freq",80+1.1**fromIntegral i),("dur",h c)]
  h x = fromIntegral (fromEnum x - 32) / 94

-- | Using pt03. Char code used for fm index value.
mkOSC2 :: String -> [OSC]
mkOSC2 ls = concat $ catMaybes $ zipWith g [0,1..] ls where
  g i c
    | c == ' '  = Nothing
    | otherwise =
      Just $ [s_new "pt03" (-1) AddToTail 1
              [("freq",f i),("idx",h c),("pan",p c),("amp",0.03)]]
  -- f x   = x * 110
  -- f x   = 80 + 1.1 ** fromIntegral x
  -- f x   = 60 + (exp (fromIntegral (fromEnum x))/24)
  f x   = 60 + 1.08 ** fromIntegral x
  h x   = fromIntegral (fromEnum x - 32) + 1
  p x   = (-1) + (fromIntegral (fromEnum x `mod` 3))

mkOSC2b :: String -> [OSC]
mkOSC2b ls = concat $ catMaybes $ zipWith g [0,1..] ls where
  g i c
    | c == ' '  = Nothing
    | otherwise =
      Just $ [s_new "pt03" (-1) AddToTail 1
              [("freq",f c),("idx",h i),("pan",p i),("amp",0.03)]]
  -- f x   = x * 110
  -- f x   = 80 + 1.1 ** fromIntegral x
  -- f x   = 60 + (exp (fromIntegral (fromEnum x))/24)
  f x   = 60 + 1.08 ** fromIntegral (fromEnum x)
  h x   = fromIntegral (x - 32) + 1
  p x   = (-1) + fromIntegral (x `mod` 3)

mkOSC3 :: String -> [OSC]
mkOSC3 ls = catMaybes $ zipWith g [0,1..] ls where
  g i c
    | c == ' ' = Nothing
    | otherwise =
      Just $ s_new "pt04" (-1) AddToTail 1
      [("freq",f i),("q",h c),("pan",p c),("amp",0.001)]
  f x = 60 + 1.08 ** fromIntegral x
  h x = fromIntegral (fromEnum x - 32) / 94
  p x = (-1) + fromIntegral (fromEnum x `mod` 3)

mkOSC4 :: String -> [OSC]
mkOSC4 ls = concat $ catMaybes $ zipWith g [0,1..] ls where
  g i c
    | c == ' ' = Nothing
    | otherwise =
      Just $ [s_new "pt04" (-1) AddToTail 1
              [("freq",f i),("q",h c),("pan",p c),("amp",0.001)]
             ,s_new "pt03" (-1) AddToTail 1
              [("freq",f i),("pan",r c),("amp",0.02),("idx",j c)]]
  f x = 60 + 1.08 ** fromIntegral x
  h x = fromIntegral (fromEnum x - 32) / 94
  p x = (-1) + fromIntegral (fromEnum x `mod` 3)
  j x = fromIntegral (fromEnum x - 32) + 1
  r x = 1 + ((-1) * fromIntegral (fromEnum x `mod` 3))

interval :: Double
interval = 3.90625e-3
-- interval = 3.125e-2
-- interval = 1.5625e-2

-- all-sorted.txt has 89723 lines.

go :: FilePath -> IO [OSC]
go path = do
  contents <- readFile path
  let ls = zip [0,1..] (lines contents)
      mko (t,cs)
        | null cs   = Nothing
        | otherwise =
          Just $ map (bundle (NTPr (t*interval))) (splitBy 64 $ mkOSC4 cs)
      ini = bundle (NTPr 0) [g_new [(1,AddToTail,0)]]
  return $ ini : (concat $ catMaybes $ map mko ls)

splitBy :: Int -> [a] -> [[a]]
splitBy i xs = case xs of
  [] -> []
  _  -> case splitAt i xs of (pre,post) -> pre : splitBy i post