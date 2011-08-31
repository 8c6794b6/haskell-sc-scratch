{-|
Module      : $Header$
CopyRight   : (c) 8c6794b6
License     : BSD3
Maintainer  : 8c6794b6@gmail.com
Stability   : unstable
Portability : non-portable

Scratch, take 2.

-}
module Sound.SC3.Lepton.Scratch.RespTest where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import System.Random

import Sound.OpenSoundControl
import Sound.SC3
import Sound.SC3.ID
import Sound.SC3.Lepton

import Sound.SC3.Lepton.Pattern.Interpreter
import Sound.SC3.Lepton.Respond hiding (setup)
import qualified Sound.SC3.Lepton.Respond as Respond

-- | Load synth def and play the pattern.
gospe :: (Transport t) => t -> IO ()
gospe fd = do
  async fd . d_recv . synthdef "speSynth" =<< speSynth
  mapPIO_ f pspe
  where
    f v = do
      send fd $ s_new "speSynth" (-1) AddToTail 1 [("freq",midiCPS v)]
      threadDelay (floor $ 0.13 * 1e6)

-- | Synthdef for spe example.
speSynth :: IO UGen
speSynth = do
  dl <- randomRs (0,0.05) `fmap` newStdGen
  dr <- randomRs (0,0.05) `fmap` newStdGen
  return $ out 0 $ mkSig dl dr
  where
    mkSig dl dr = foldr f v (take 4 $ zipWith mce2 dl dr)
    v = rlpf (lfSaw AR freq 0 * evl) nz 0.1
    f a b = allpassN b 0.05 a 4
    evl = envGen KR 1 1 0 1 RemoveSynth shp * 0.3
    shp = envPerc 10e-3 1
    nz = midiCPS (lfNoise1 'z' KR 1 * 36 + 110)
    freq = control KR "freq" 440

pspe =
  pcycle
    [prand 1
       [pempty, plist [24,31,36,43,48,55]]
    ,pseq (prange 2 5)
       [60, prand 1 [63, 65], 67, prand 1 [70,72,74]]
    ,prand (prange 3 9)
       [74,75,77,79,81]]

------------------------------------------------------------------------------
-- Sine and whitenoises

gosw :: Transport t => t -> IO ()
gosw fd = do
  setup fd
  send fd $ s_new "rspdef3" 1003 AddToHead 1 []
  runMsg (ppar [loop01, loop02, loop03]) fd

gosw2 :: IO ()
gosw2 =
  bracket
    (mapM (forkIO . w . runMsg) [loop02,loop03])
    (mapM_ killThread)
    (const $ w $ runMsg loop01)

setup :: Transport t => t -> IO OSC
setup fd = do
  Respond.setup fd
  async fd $ bundle immediately
    [d_recv $ synthdef "rspdef1" rspdef1
    ,d_recv $ synthdef "rspdef2" rspdef2
    ,d_recv $ synthdef "rspdef3" rspdef3
    ,d_recv $ synthdef "rspdef4" rspdef4
    ,d_recv $ synthdef "rspdef5" rspdef5]

rspdef1 :: UGen
rspdef1 =
  out 0 $ pan2
  (sinOsc AR ("freq"@@440 * ("fmul"@@1 `lag2` 3.5)) 0 * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef2 :: UGen
rspdef2 =
  out 0 $ pan2
  (resonz (whiteNoise 'd' AR)
   ("freq"@@1320)
   (clip ("q"@@0.8 * mouseY KR 0.125 4 Exponential 0.1) 1e-5 9999e-4) * 0.3 *
   envGen KR ("t_trig"@@1) 1 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvSin] (-1) 0))
  ("pan"@@0) ("amp"@@1)

rspdef3 :: UGen
rspdef3 = out ("out"@@100) (tExpRand 'f' 0.25 4 ("t_trig"@@1))

rspdef4 :: UGen
rspdef4 = out 0 $
  lfPar AR ("freq"@@440 `lag` 0.25) 0 * ("amp"@@0.3 `lag3` 0.3)

-- | Variant of 'rspdef1', using 'in\'' ugen to map frequency factor.
rspdef5 :: UGen
rspdef5 =
  out 0 $ pan2
  (sinOsc AR ("freq"@@440 * (in' 1 KR ("fmul"@@100) `lag2` 3.5)) 0 *
   envGen KR ("t_trig"@@1) 0.3 0 1 RemoveSynth
   (env [0,1,0] [("atk"@@1e-4),("dcy"@@999e-4)] [EnvCub] (-1) 0))
  ("pan"@@0) ("amp"@@1)

loop01 :: Msg Double
loop01 = snew "rspdef1" Nothing AddToTail 1
  [("dur",

    -- pforever (1/33))

    pcycle [preplicate 1024 (1/23)
           ,preplicate 512 (2/23)
           ,preplicate 256 (4/23)
           ,preplicate 128 (8/23)])

  ,("freq", fmap midiCPS $ pforever $ prand 1 $
            [40,41,48,52,55,58,62,67,70,74,79,86,90])
  ,("pan",  pforever $ prange (-1) 1)
  ,("atk",  pforever $ prange 1e-4 1)
  ,("dcy",  pforever $ prange 1e-2 1)
  ,("amp",  pforever $ prange 1e-3 1)
  -- ,("fmul", pforever 100)
  ,("n_map/fmul", pforever 100)]

loop02 :: Msg Double
loop02 = snew "rspdef2" Nothing AddToTail 1
  [("dur",  pforever $ prange 1e-1 5e-1)
  ,("freq", pforever $ exp <$> prange (log <$> 110) (log <$> 11000))
  ,("atk",  pforever $ prange 1e-4 2)
  ,("dcy",  pforever $ prange 1e-4 2)
  ,("amp",  pforever $ prange 1e-2 1)
  ,("pan",  pforever $ prange (-1) 1)
  ,("q",    pforever $ prange 1e-3 99e-2)]

loop03 :: Msg Double
loop03 = mkNset 1003
  [("dur",    pforever $ prange 4 32)
  ,("t_trig", pforever 1)]

loop04 :: R (ToOSC Double)
loop04 = snew "rspdef1" Nothing AddToTail 1
  [("dur",  pforever $ prange 1e-3 7.5e-2)
  ,("freq", pforever $ exp <$> prange (log <$> 80) (log <$> 12000))
  ,("atk",  let xs = take 1024 $ iterate (*1.006) 0.002
            in  pcycle $ fmap pval (xs ++ reverse xs))
  ,("dcy",  pforever $ prange 1e-4 2e-1)
  ,("amp",  pforever $ prange 1e-1 3e-1)
  ,("pan",  pforever $ prange (-1) 1)]

msg01 :: Msg Double
msg01 = snew "rspdef1" Nothing AddToTail 1
  [("dur", pforever 20e-3)
  ,("freq", preplicate 200 8000)
  ,("atk", pforever 1e-3)
  ,("dcy", pforever 10e-3)
  ,("amp", pforever 0.3)]

main = w gosw

-- | Send 's_new' message using pattern.
-- sNew :: Transport t
--   => AddAction
--   -- ^ Add action for s_new message
--   -> Int
--   -- ^ Node id of add target
--   -> String
--   -- ^ Synthdef name
--   -> [(String,R Double)]
--   -- ^ Param name and pattern for the param, passed to 'mkOpts'.
--   -> t -> IO ()
-- sNew aa tid def ps fd = join $ foldM_ f <$> utcr <*> k ps where
--   k = runPIO . V.fromList . T.sequenceA . M.fromList
--   f t0 m = do
--     nid <- newNid
--     let dt = M.findWithDefault 1 "dur" m
--         (opts,ps) = M.partitionWithKey (\k _ -> '/' `elem` k) m
--     send fd $ bundle (UTCr $ t0+dt+offsetDelay) $
--       s_new def nid aa tid (M.assocs ps) : M.foldrWithKey (mkOpts nid) [] opts
--     waitUntil fd "/n_go" nid
--     return (t0+dt)
-- {-# SPECIALISE sNew ::
--    AddAction -> Int -> String -> [(String,R Double)] -> UDP -> IO () #-}
-- {-# SPECIALISE sNew ::
--    AddAction -> Int -> String -> [(String,R Double)] -> TCP -> IO () #-}
--

-- | Send 'n_set' message using pattern.
-- nSet :: Transport t
--   => AddAction
--   -- ^ Add action
--   -> Int
--   -- ^ Target node id of AddAction
--   -> String
--   -- ^ Synthdef name
--   -> [(String,R Double)]
--   -- ^ Pair of parameter and value
--   -> t -> IO ()
-- nSet aa tid def pms fd = do
--   nid  <- newNid
--   trid <- newNid
--   send fd $ bundle immediately
--     [s_new def nid aa tid [],s_new "tr" trid AddBefore nid []]
--   join $ foldM_ (f nid trid) <$> utcr <*> k pms
--   where
--     k = runPIO . T.sequenceA . M.fromList
--     f nid trid t0 m = do
--       let dt = M.findWithDefault 1 "dur" m
--       send fd $ bundle (UTCr $ t0+dt+offsetDelay)
--         [n_set nid (M.assocs m),n_set trid [("t_trig",1)]]
--       waitUntil fd "/tr" trid
--       return (t0+dt)
-- {-# SPECIALISE nSet ::
--    AddAction -> Int -> String -> [(String,R Double)] -> UDP -> IO () #-}
-- {-# SPECIALISE nSet ::
--    AddAction -> Int -> String -> [(String,R Double)] -> TCP -> IO () #-}

