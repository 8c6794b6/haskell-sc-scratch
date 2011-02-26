------------------------------------------------------------------------------
-- Scratch for A007
--

:cd src
:load Sound.Study.ForAPileOfOscillators.A007

:m + Sound.SC3 Sound.SC3.Lepton Sound.OpenSoundControl Control.Concurrent
:set -XNoMonomorphismRestriction

w reset
w setup

e <- initEnvTU 0.42
dumpEnv e

tpause (tu 8) e "a01"
tresume (tu 8) e "a01"
tkill (tu 4) e "a01"

tpause (tu 8) e "a02"
tresume (tu 8) e "a02"
tkill (tu 4) e "a02"

tkill (tu 4) e "a01"
tpause (tu 4) e "a01"
tresume (tu 4) e "a01"

tkill (tu 4) e "a08"
tpause (tu 4) e "a08"
tresume (tu 4) e "a08"

tupdate (tu 4) e "a01" $ nsetP oc71Id
  [("t_trig",
    pforever (pchoose 1 [1,1,1,1,1,1]))
  ,("edgey",
    pcycle [2e-3, pchoose (prange 1 15) [998e-3,993e-3,998e-3,997e-3]])
  ,("del",
    pforever (pchoose 1 [1/2, 1/4, 1/4, 1/4]))
    {- pforever (pchoose 1 [1/2, 1/4,1/4
                        ,plist [1/6,1/6,1/6]
                        ,plist [1/8,1/8] ])) -}
  ,("mamp",
    pforever (prange 3e-2 4e-2))]

tkill (tu 4) e "a02"
tpause (tu 8) e "a02"
tresume (tu 8) e "a02"

tupdate (tu 4) e "a02" $ nsetP oc72Id
  [("t_trig",
    pforever 1)
  ,("edgey",
    pforever (plist [5e-3, 500e-3,900e-3, 5e-3, 500e-3]))
  ,("del",
    pforever (plist [1, 1/2, 1/2, 3/2, 1/2]))
  ,("mamp",
    pcycle [4e-2, pchoose 4 [prange 2e-2 3.5e-2]])]

nChg <- newMVar (32::Int)
nThr <- newMVar (32::Int)
tupdate (tu 8) e "a08" (goRec3 nChg nThr)
tpause (tu 8) e "a08"
tresume (tu 8) e "a08"
tkill (tu 8) e "a08"

modifyMVar_ nChg (const $ return 32)
modifyMVar_ nThr (const $ return 32)
readMVar nChg
readMVar nThr

tpause (tu 4) e "a04"
tresume (tu 4) e "a04"

tupdate (tu 4) e "a04" $ do
  { is <- act $ runPIO $ pchoose (prange 1 128) (map pval [0..255])
  ; cf <- act $ runPIO $ prange 4.2 10
  ; fs <- act $ runPIO $ pchoose 1
          [pforever $ prange 4.2 10
          ,pforever $ prange 8.4 10
          ,pforever $ prange 4.2 5
          ,pforever $ pval $ head cf]
  ; rs <- act $ runPIO $ pchoose 1 [0.25, 0.25, plist [0.25, 0.25], 0.5, 0.5, 8]
  ; now <- getNow
  ; act $ w $ flip send $ Bundle (UTCr (now+0.1))
      [b_set pitchBuf $ zip is (map exp fs)
      ,n_set pc72Id [("t_trig", 1)]]
  ; rest $ head rs }

w $ flip send $ n_set oc71Id [("dmax",88e-3)]
w $ flip send $ n_set oc72Id [("dmax",84e-3)]
w $ flip send $ n_set pc72Id [("lagt",5e-3)]

w $ flip send $ n_set pc72Id [("t_trig",1)]
w $ flip send $ n_set ac71Id [("amp",0.000)]
w $ flip send $ n_set ac72Id [("amp",0.035),("freq",0.00),("edgey",990e-3),("dmax",4440e-3)]

w $ flip send $ n_set ac73Id [("amp",0.033),("freq",0.00),("edgey",5e-3),("dmax",500e-3)]

mapM_ (tkill0 e) ["a01","a02","a03","a04","a05","a06","a07","a08", "z01"]

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

tupdate (tu 4) e "a02" $ nsetP oc72Id
  [("t_trig",
    pforever (pchoose 1 [1,1,1,1,1,1,1,0]))
  ,("edgey",
    pcycle [2e-3, pchoose (prange 1 15) [998e-3,993e-3,998e-3,997e-3]])
  ,("del",
    pforever (pchoose 1 [1, 1/2, 1/2, 1/2]))
  ,("mamp",
    pforever (prange 3e-2 4e-2))]

w $ flip send $
  Bundle (NTPi 0) [b_set pitchBuf $ zip [2,4..256] [80,128..]
                  ,n_set pc72Id [("t_trig",1)]]

tpause (tu 4) e "z01"
tresume (tu 8) e "z01"
tkill (tu 8) e "z01"

tupdate (tu 8) e "z01" $ do
  { name <- act $ runPIO $ pchoose 1 $
            map pval ["a01", "a02", "a03", "a04", "a05", "a06", "a07", "a08"]
  ; r1 <- act $ runPIO $ pchoose 1 [4, 8]
  ; r2 <- act $ runPIO $ pchoose 1 [0, 4, 8]
  ; act $ tpause (tu 4) e $ head name
  ; rest $ head r1
  ; act $ tresume (tu 4) e $ head name
  ; rest $ head r2 }

tkill0 e "a03"
tpause (tu 4) e "a03"
tresume (tu 4) e "a03"
tupdate (tu 4) e "a03" $ do
  { fs <- act $ runPIO (pchoose 1
                        [pforever $ prange 8.4 10
                        ,pforever $ prange 4.2 10
                        ,pforever $ prange 5 7
                        ,pforever (pval $ log 440)])
  ; rs <- act $ runPIO (pchoose 1 [0.5, 0.75])
  ; now <- getNow
  ; act $ withSC3 $ flip send $
    Bundle (UTCr (now+0.1)) [b_setn pitchBuf [(0,fmap exp (take numOsc fs))]
                            ,n_set pc72Id [("t_trig",1)]]
  ; rest $ head rs }

tkill (tu 4) e "a03"
tpause (tu 4) e "a03"
tresume (tu 4) e "a03"

w $ flip send $ Bundle (NTPi 0)
  [b_setn pitchBuf [(0,take numOsc $ repeat 440)]
  ,n_set pc72Id [("t_trig",1)]]

do { fs <- runPIO (pforever (prange 8 10))
   ; w $ flip send $ c_setn [(head fBusses, fmap exp (take numOsc fs))] }

tpause (tu 2) e "a04"
tresume (tu 2) e "a04"

tkill0 e "a04"
tupdate (tu 2) e "a04" $ do
  { fs <- act $ runPIO $
          pchoose 1 [pval $ take numOsc [17,34..]
                    ,pval $ take numOsc $ cycle $ map midiCPS
                     [36,40,43,48,52,55,60,64,67,72,76,79,84]
                    ,pval $ take numOsc $ cycle $ map midiCPS
                     [36,39,43,48,51,55,60,63,67,72,75,79,84]
                    ,pval $ take numOsc $ fmap exp [8,8+(2/256)..10]
                    ,pval $ take numOsc (repeat 440)]
  ; act $ w $ flip send $ c_setn [(head fBusses, head fs)]
  ; rest 4 }

tkill (tu 4) e "a04"
tupdate (tu 2) e "a04" $ do
  { fs <- act $ runPIO $
          pchoose 1 [pval $ take numOsc $ cycle $ map midiCPS
                     [36,40,43,48,52,55,60,64,67,72,76,79,84,88,91,96]
                    ,pval $ take numOsc $ cycle $ map (midiCPS . (+5))
                     [31,36,39,43,48,51,55,60,63,67,72,75,79,84,87,91,96]
                    ,pval $ take numOsc $ cycle $ map midiCPS
                     [36,43,48,55,60,67,72,79,84,91,96]
                    ,pval $ take numOsc $ cycle $ map midiCPS
                     [36,41,48,53,60,65,72,77,84,89,96]]
  ; now <- getNow
  ; act $ w $ flip send $
    Bundle (UTCr (now+0.1)) [c_setn [(head fBusses, head fs)]]
  ; rest 4 }

tkill (tu 4) e "a05"
tupdate (tu 4) e "a05" $ do
  { base <- act $ runPIO (prange 20 27)
  ; interval <- act $ runPIO (pchoose 1 [1,3,6,7])
  ; act $ pf1 (head interval) (head base)
  ; rest 8 }

tkill (tu 4) e "a06"
tupdate (tu 4) e "a06" $ do
  { act $ runPIO (pforever (prange 4.5 10)) >>= setPitchBuf . map exp . take numOsc
  ; rest 4 }

tpause (tu 8) e "a07"
tresume (tu 8) e "a07"

:m + System.Random
do { fs <- return . randomRs (30::Int, 127) =<< newStdGen
   ; setPitchBuf (take numOsc $ map fromIntegral fs) }

pf1 1 28
pf1 2 28 -- whole tone
pf1 2 29 -- whole tone
pf1 3 28 -- diminish
pf1 4 28
pf1 5 28
pf1 6 28
pf1 7 22
pf1 7 24
pf1 7 26
pf1 8 28
pf1 9 28
pf1 10 28
pf1 11 28
pf1 12 25

pf1 3 28
pf1 3 29
pf1 3 30

pf1 4 28
pf1 4 29
pf1 4 30
pf1 4 31

:load Sound.Study.ForAPileOfOscillators.A007
w reset
w setup
w $ printNode 11
w $ \fd -> send fd (s_get (head $ drop 32 $ oscIds) ["freq"]) >> wait fd "/n_set"
w $ \fd -> send fd (c_setn [(ampBus (head oscIds),take numOsc $ repeat 0.002)])
w $ \fd -> send fd (c_setn [(ampBus (head oscIds),take numOsc $ repeat 0.0)])
w $ \fd -> send fd (c_set [(1004,0.0)])
w $ \fd -> send fd $ n_set oc71Id [("pan",0.4)]
:m + System.Random

map ampBus oscIds

w $ \fd -> send fd (b_query [pitchBuf]) >> wait fd "/b_info"
w reset

setPitchBuf $ (take numOsc $ cycle [48,55,62,69])