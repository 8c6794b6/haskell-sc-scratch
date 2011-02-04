:m + Sound.SC3 Sound.SC3.ID Sound.SC3.Lepton Sound.OpenSoundControl
:set -XNoMonomorphismRestriction
:set -fobject-code

let sdef fd n u = async fd . d_recv $ synthdef n u

withSC3 $ \fd -> do
  { sdef fd "foo"
    (out 0 $ sinOsc ar 440 0 * 0.3 * xLine kr 1 1e-5 200e-3 RemoveSynth)
  ; sdef fd  "bar"
    (out 1 $ sinOsc ar 330 0 * 0.3 * xLine kr 1 1e-5 200e-3 RemoveSynth)
  ; sdef fd "xx1"
    (out 0 $ pan2
     (sinOsc ar ("freq"=:440) 0 * 0.3 *
      xLine kr 1 1e-5 ("dur"=:200e-3) RemoveSynth)
     ("pan"=:0) 1)
  ; sdef fd "xx2"
    (out 0 $ pan2
     (hpf (whiteNoise 'd' ar) 3809.3 *
      envGen kr 1 1 0 1 RemoveSynth (envPerc 5e-3 80e-3) * ("amp"=:0.3))
     ("pan"=:0) 1)
  ; sdef fd "xx3"
    (out 0 $ pan2
     ((sinOsc ar (xLine kr 80 40 200e-3 DoNothing) 0 +
       impulse ar (xLine kr 100 0 120e-3 DoNothing) 1)*
      envGen kr 1 1 0 1 RemoveSynth
      (env [0,0,1,1,0] [0,2e-3,80e-3,180e-3] [EnvNum (-13)] (-1) 0) * 0.6)
     ("pan"=:0) 1)
  ; sdef fd "xx4"
    (out 0 $ pan2
     (resonz (whiteNoise 's' ar * 2 +
              pulse ar (xLine kr 1200 2800 1300e-3 DoNothing) 0.5) 3800 0.9 *
      decay2 (dust 'd' kr (xLine kr 20 2 2 DoNothing) +
              impulse kr (xLine kr 8 4 2 DoNothing) 0) 1e-3 80e-3 *
      envGen kr 1 1 0 1 RemoveSynth
      (env [0,0,1,1,0] [0,5e-3,80e-3,1800e-3] [EnvNum (-10)] (-1) 0) * 0.8)
     ("pan"=:0) 1)
  ; sdef fd "buzz"
    (out 1 $ sinOsc ar 999 0 * 0.3 * xLine kr 1 1e-5 200e-3 RemoveSynth)}

withSC3 $ \fd -> send fd $ s_new "xx2" (-1) AddToTail 1 []
withSC3 $ \fd -> send fd $ s_new "xx3" (-1) AddToTail 1 []
withSC3 $ \fd -> send fd $ s_new "xx4" (-1) AddToTail 1 []

e <- initEnv

let newXX1 f p t = withSC3 $ flip send $
    Bundle (UTCr $ t+0.1)
      [s_new "xx1" (-1) AddToTail 1 [("freq",f),("pan",p)]]
let newXX2 p a t = withSC3 $ flip send $
    Bundle (UTCr $ t+0.1)
      [s_new "xx2" (-1) AddToTail 1 [("pan",p),("amp",a)]]
let newXX3 p t = withSC3 $ flip send $
    Bundle (UTCr $ t+0.1)
      [s_new "xx3" (-1) AddToTail 1 [("pan",p)]]
let newXX4 p t = withSC3 $ flip send $
    Bundle (UTCr $ t+0.1)
      [s_new "xx4" (-1) AddToTail 1 [("pan",p)]]

newXX2 0 0.3 =<< utcr
newXX3 0 =<< utcr
newXX4 0 =<< utcr

-- let hat = "8 1 8 8  8 1 1 3"
-- let kil = ". . . 5  0 . . ."
-- let snr = "0 . . .  9 . . ."

taddAt (atTU 4) e "tmtB01" $ do
  { tu <- getTimeUnit
  ; now <- getNow

  ; act $ newXX2 0.5 0.4 now
  ; tdelay (tu*0.5)
  ; act $ newXX2 0.5 0.1 (now+tu*0.5)
  ; tdelay (tu*0.25)
  ; act $ newXX2 0.5 0.08 (now+tu*0.75)
  ; tdelay (tu*0.25)
  ; act $ newXX2 0.5 0.28 (now+tu*1)
  ; tdelay (tu*0.5)
  ; act $ newXX2 0.5 0.1 (now+tu*1.5)
  ; tdelay (tu*0.5)

  ; act $ newXX2 0.5 0.38 (now+tu*2)
  ; tdelay (tu*0.5)
  ; act $ newXX2 0.5 0.08 (now+tu*2.5)
  ; tdelay (tu*0.5)
  ; act $ newXX2 0.5 0.12 (now+tu*3)
  ; tdelay (tu*0.5)
  ; act $ newXX2 0.5 0.08 (now+tu*3.5)
  ; tdelay (tu*0.5) }

tkill e "tmtB01"

taddAt (atTU 4) e "tmtB02" $ do
  { tu <- getTimeUnit
  ; now <- getNow
  ; act $ newXX3 (-0.3) now
  ; rest 1.5
  ; act $ newXX3 (-0.3) (now+tu*1.5)
  ; rest 0.5
  ; act $ newXX3 (-0.3) (now+tu*2)
  ; rest 2 }

taddAt (atTU 4) e "tmtB03" $ do
  { now <- getNow
  ; tu <- getTimeUnit
  ; rest 3;
  ; act $ newXX4 0 (now+tu*3)
  ; rest 1; }

tkill e "tmtB01"

mapM_ (tkill e) ["tmtB01", "tmtB02", "tmtB03"]

dumpEnv e
setTimeUnit e 1

tpauseAt (atTU 2) e "tmtB01"
tresumeAt (atTU 2) e "tmtB01"

tpauseAt (atTU 4) e "tmtB02"
tresumeAt (atTU 4) e "tmtB02"

tpauseAt (atTU 4) e "tmtB03"
tresumeAt (atTU 4) e "tmtB03"

taddAt (atTU 1) e "tmtA01" $ do
  { now <- getNow
  ; act $ newXX1 440 0 now
  ; rest 1 }

taddAt (atTU 1) e "tmtA02" $ do
  { now <- getNow
  ; act $ newXX1 440 1 now
  ; rest 1 }

taddAt (atTU 1) e "tmtA03" $ do
  { now <- getNow
  ; act $ newXX1 440 (-1) now
  ; rest 1 }

setTimeUnit e 1
dumpEnv e

mapM_ (tkill e) ["tmtA01", "tmtA02", "tmtA03"]

tkill e "tmtA01"
tkill e "tmtA02"
tkill e "tmtA03"

tpause e "tmtA01"
tpause e "tmtA02"
tpause e "tmtA03"

tpauseAt (atTU 1) e "tmtA01"
tresumeAt (atTU 1) e "tmtA01"

tpauseAt (atTU 1) e "tmtA02"
tresumeAt (atTU 1) e "tmtA02"

tpauseAt (atTU 1) e "tmtA03"
tresumeAt (atTU 1) e "tmtA03"

tresume e "tmtA01"
tresume e "tmtA02"
tpause e "tmtA02"
tresume e "tmtA03"

:m + System.Random
sequence_ $ replicate 30 $ do
  { now <- utcr
  ; dur <- randomRIO (20e-3, 300e-3)
  ; pause <- randomRIO (1e3, 5e4)
  ; withSC3 $ flip send $ Bundle (UTCr $ now+0.1)
      [s_new "xx1" (-1) AddToTail 1 [("freq",8000),("dur",dur)]]
  ; threadDelay (floor $ pause) }

tadd e "foo" $ act $ do
  { withSC3 $ \fd -> send fd $ s_new "foo" (-1) AddToTail 1 []
  ; threadDelay (5*10^5) }

tadd e "bar" $ do
  { act $ withSC3 $ \fd -> send fd $ s_new "bar" (-1) AddToTail 1 []
  ; tdelay 0.5 }

tadd e "buzz" $ do
  { tu <- getTimeUnit
  ; act $ withSC3 $ \fd -> send fd $ s_new "buzz" (-1) AddToTail 1 []
  ; tdelay 0.5 }

tkill e "foo"
tkill e "bar"
tkill e "buzz"

tpause e "foo"
tresume e "foo"

tpause e "bar"
tresume e "bar"

tpause e "buzz"
tresume e "buzz"

(u0,t1) <- a1
(u2,t2) <- a2

mapM_ killThread [t1,t2]

-- With Pinger by rd.
:m + Control.Concurrent
t1 <- forkIO (pinger 440 0.1 1)
t2 <- forkIO (pinger 440 0.1 (-1))
t3 <- forkIO (pinger 880 0.1 0.3)
t4 <- forkIO (pinger 1320 0.1 (-0.3))
mapM_ killThread [t1,t2,t3,t4]

withSC3 $ \fd -> async fd $ d_recv $ synthdef "ping" $ ping

e <- initEnv

tadd e "tmtC01" $ do
  { act $ print "start"
  ; rest 1
  ; act $ print "after resting for 1 tu"
  ; rest 1
  ; act $ print "rest another 1 tu"
  ; rest 1 }

tkill e "tmtC01"

e <- initEnv
dumpEnv e
tadd e "tmtC02" $ forever $ do
  { del <- act $ randomRIO (0.25,1)
  ; act $ putStrLn $ "delaying for " ++ show del
  ; breakMe
  ; rest del }

tpause e "tmtC02"
tresume e "tmtC02"

tkill e "tmtC02"
