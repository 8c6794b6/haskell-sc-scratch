------------------------------------------------------------------------------
--
-- This code won't compile, it's use is to select each block of code and
-- send to ghci interactively.
--
-- Select block of code (let or do), and hit C-M-x from emacs.
-- Or, each line could be sent to ghci with C-c C-C.
--
-- Requires inf-haskell-send-region and inf-haskell-send-line.
--
------------------------------------------------------------------------------

-- Import modules
:m + Sound.OpenSoundControl Sound.SC3 Sound.SC3.ID Sound.SC3.Lepton
:m + Control.Concurrent Control.Monad Data.List
:set -XNoMonomorphismRestriction

-- sdef :: String -> UGen -> IO ()
let sdef name ug = withSC3 $ \fd -> do
  { async fd (d_recv $ synthdef name ug) }

-- hatch :: String -> [(String, Double)] -> IO ()
let hatch name ps = withSC3 $ \fd -> do
  { send fd (s_new name (-1) AddToTail 1 ps) }

-- snew :: String -> AddAction -> Int -> [(String, Double)] -> IO ()
let snew name aaction target ps = withSC3 $ \fd ->
  send fd (s_new name (-1) aaction target ps)

let st = withSC3 $ \fd -> do
      { send fd status
      ; wait fd "/status.reply" }

-- Where can be used inside let, with ";" at the end of each clause.

let y1 = pan2 sig ("pan"=:0) 1 where
      sig = decay2 tr 1e-9 380e-3 * pulse ar freq pw * 0.3 * release;
      freq = ("freq"=:880);
      tr = impulse kr 1 (rand 'r' 0 1) +
           dust 'd' kr (lfdNoise3 'n' kr 0.5 * 0.5 + 0.5);
      pw = lfdNoise3 'd' kr 2.137 * 0.5 + 0.5;
      release = xLine kr 1 1e-3 ("rel"=:20) RemoveSynth;

audition $ out 0 y1

let z1 = pan2 sig ("pan"=:0) 1 where
      sig = xLine kr 1 1e-4 120e-3 RemoveSynth *
            (pulse ar ("freq"=:79) 0.2 +
             whiteNoise 'z' ar) * 0.3

audition $ out 0 z1

let z2 = pan2 sig ("pan"=:0) 1 where
      sig = envGen kr 1 1 0 1 RemoveSynth shape *
            sinOsc ar (xLine kr 80 20 100e-3 DoNothing)
            (line kr 0 pi 180e-3 DoNothing) * 0.8;
      shape = env [0,0,1,1,0] [0,5e-3,20e-3,280e-3] [EnvNum (-10)] (-1) 0

audition $ out 0 z2

let z3 = pan2 sig ("pan"=:0) 1 where
      sig = envGen kr 1 1 0 1 RemoveSynth shape *
            sinOsc ar (("freq"=:7809.8)*fenv) 0 *
            decay2 (dust 'd' kr dfreq) 1e-3 20e-3;
      shape = env [0,0,1,0] [0,5e-3,800e-3] [EnvNum (-10)] (-1) 0;
      dfreq = line kr 62 0 1 DoNothing;
      fenv = xLine kr 1 0.25 1 DoNothing

audition $ out 0 z3

let m1 = mrg [replaceOut 0 (in' 1 ar 0*a),replaceOut 1 (in' 1 ar 1*a)] where
      a = ("amp"=:1) `lag` ("lag"=:2)

-- Send synthdefs
mapM (uncurry sdef)
  [("y1",(out 0 y1)),("m1",m1)
  ,("z1",(out 0 z1)),("z2",(out 0 z2)),("z3",(out 0 z3))]

-- Group mapping
let g = Group 1
        [Group 11 [Synth (-1) "m1" ["amp":=0.8]]
        ,Group 10 []]
in  withSC3 $ addNode 0 g

-- Now I understand why rd was wrting like this.
let a1 pch = do
      { ds <- runPIO $ pforever $ pchoose 1 $ take 16
               (zipWith (+) (cycle [0,7,12])
                (concatMap (replicate 3) [0,12..]))
      ; ps <- runPIO $ pforever (prange (-1) 1)
      ; rs <- runPIO $ pforever (pchoose 1 [8,10..25])
      ; let f d p r = snew "foo" AddToHead 11
                   [("freq", midiCPS (d+pch)),("pan", p),("rel",r)]
      ; sequence_ $ zipWith3 f (take 16 ds) ps rs }

let ampa1 val = withSC3 $ \fd -> send fd $ n_set 11 [("amp",val)]
ampa1 0.2

-- Play 3 a1 actions together.
mapM_ a1 [46,41,36]
mapM_ a1 [41,36,31]
mapM_ a1 [36,31,26]
mapM_ a1 [36,34,32]
mapM_ a1 [38,36,34]
mapM_ a1 [34,32,30]

-- Sequence a1s.
let b1 fac = sequence_ $
    zipWith (\ps t -> mapM_ a1 ps >> threadDelay (floor $ fac * t * 1e6))
    [[46,41,36], [41,36,31], [36,31,26], [36,34,32]]
    [3, 5, 4, 4]

-- Fork the sequence, hold ThreadId so that it could be killed later.

e <- initEnv 1

t1 <- forkIO $ forever (b1 1.6)
killThread t1

let b2 = do
      { hs <- runPIO $ pforever $ pcycle [0,0,1,0]
      ; bs <- runPIO $ pforever $ pseq 1 [1,0,0,pchoose 1 [0,1]]
      ; let f h b | b == 1 = [s_new "z2" (-1) AddToHead 10 [("pan",0.2)]]
                  | h == 1 = [s_new "z1" (-1) AddToHead 10 [("pan",-0.1)]]
                  | otherwise = [s_new "z3" (-1) AddToHead 10 [("pan",-0.2)]]
      ;     g m = withSC3 (\fd -> send fd (Bundle immediately m) >>
                                  threadDelay (floor $ 4 * 1e5))
      ; in  forM_ (zipWith f hs bs) g }

t2 <- forkIO b2
killThread t2
