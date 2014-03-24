----------------------------------------------------------------------
-- |
-- Module      : $Header$
-- CopyRight   : (c) 8c6794b6
-- License     : BSD3
-- Maintainer  : 8c6794b6@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- Some reusable functions not fitting to elsewhere module.
--
module Sound.SC3.Lepton.Util where

import Control.Monad.IO.Class
import Data.Int (Int32)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy as B

import Sound.SC3
import Sound.SC3.UGen.MCE
import Sound.OSC

-- | Environmental variable for synthdefs.
scSynthdefPath :: String
scSynthdefPath = "SC_SYNTHDEF_PATH"

-- | This function writes the synthdef file to directory specifyed by
-- environmental variable @SC3_SYNTHDEF_PATH@. Though, to use the
-- synthdef, one must load the synthdef file explicitly, or execute
-- @withSC3 reloadSynthdef@ action.
writeSynthdef :: String -> UGen -> IO ()
writeSynthdef name ugen = do
  envir <- getEnvironment
  let dir = fromMaybe "./" $ lookup scSynthdefPath envir
      path = dir </> name <.> "scsyndef"
      def = Synthdef name (synth ugen)
      contents = synthdefData def
  B.writeFile path contents

-- | Reload synthdef directory. If specified, path to the
-- @HSC3_SYNTHDEF_DIR@ would be used. Otherwise, current directory
-- would be used.
reloadSynthdef :: (MonadIO m, DuplexOSC m) => m Message
reloadSynthdef = do
  envir <- liftIO $ getEnvironment
  let dir = fromMaybe "./" $ lookup scSynthdefPath envir
      path = dir </> ""
  async (d_loadDir path)

-- | Type synonym for sending osc message to scsynth.
type SendUDP a = UDP -> IO a

-- | Get rate, name, and default value of controls from given ugen.
getControls :: UGen -> [(Rate,String,Float,Bool)]
getControls = nub . getControls'

getControls' :: UGen -> [(Rate,String,Float,Bool)]
getControls' ug =
    case ug of
      Control_U (Control rate name def trg)     -> [(rate,name,def,trg)]
      Primitive_U (Primitive  _ _ inputs _ _ _) ->
          concatMap getControls' inputs
      Proxy_U (Proxy prim _)                    ->
          getControls' (Primitive_U prim)
      MCE_U m                                   ->
          case m of
              MCE_Unit u    -> getControls' u
              MCE_Vector us -> concatMap getControls' us
      MRG_U (MRG l r)                           ->
          getControls' l ++ getControls' r
      _                                         -> []

-- | Datatype for representing '/b_info' message returned by sending
-- '/b_query'.
data BufInfo = BufInfo {
      bufNumber :: Int32,
      bufNumFrames :: Int32,
      bufNumChannels :: Int32,
      bufSampleRate :: Float
    } deriving (Eq, Show)

-- | Send /b_query and returns BufInfo.
getBufInfo :: DuplexOSC m => Int -> m BufInfo
getBufInfo bufId = do
    msg <- send (b_query [bufId]) >> waitReply "/b_info"
    case msg of
        Message "/b_info" [Int32 bid,Int32 nf,Int32 nc,Float sr] ->
            return $ BufInfo bid nf nc sr
        _ -> error "Not a /b_info message"

queryRootNode :: DuplexOSC m => m Message
queryRootNode = withNotifications $ do
    send $ g_queryTree [(0,True)]
    waitReply "/g_queryTree.reply"

-- | Sends registration for notify, then query the nodes and shows
-- returing message.
-- queryNode :: Transport t => Int -> t -> IO OSC
queryNode :: DuplexOSC m => Int -> m Message
queryNode n = withNotifications $ do
    send (n_query [n])
    waitReply "/n_info"

-- | Dumps root node and show in scsynth.
dumpRootNode :: SendOSC m => m ()
dumpRootNode = send $ g_dumpTree [(0,True)]

-- | Sends a synthdef to server.
sendSynthdef :: DuplexOSC m => String -> UGen -> m Message
sendSynthdef name ugen = async $ d_recv $ synthdef name ugen

-- | Write a synthdef to file, then load it.
loadSynthdef :: String -> UGen -> IO Message
loadSynthdef name ugen = do
  writeSynthdef name ugen
  withSC3 $ sendSynthdef name ugen


-- | Sends @/b_get@ message and wait until it gets @/b_set@ message.
b_get' :: DuplexOSC m => Int -> [Int] -> m Message
b_get' bId is = do
    send (b_get bId is)
    waitReply "/b_set"

-- | Sends @/b_getn@ message and wait until it gets @/b_setn@.
b_getn' :: DuplexOSC m => Int -> [(Int,Int)] -> m Message
b_getn' bid params = do
    send (b_getn bid params)
    waitReply "/b_setn"


{-
-- | Write to buffer from List.
b_loadList :: Int -> Int -> Int -> [Double] -> IO OSC
b_loadList  = undefined

-- | Write to buffer from @[ByteString]@
b_loadByteString :: Int -> Int -> Int -> [B.ByteString] -> IO OSC
b_loadByteString = undefined

-- | Read from buffer to @[Double]@.
b_loadToDouble :: Int -> IO [Double]
b_loadToDouble = undefined

-- | Read from buffer to @[ByteString]@.
b_loadToByteString :: Int -> IO [B.ByteString]
b_loadToByteString = undefined

-- | Plays the buffer. Takes buffer id and boolean for looping or not.
b_play :: Int -> Bool -> IO ()
b_play _ _ = undefined
-}

{-
-- | Sends @/c_get@ message and wait until it gets @/c_set@.
c_get' :: Transport t => [Int] -> t -> IO OSC
c_get' ids = \fd -> send fd  (c_get ids) >> wait fd "/c_set"

-- | Fit in specified range with using @mod@.
fitInRange :: Integral a => a -> a -> a -> a
fitInRange lo hi target
    | target < lo = fromIntegral (target `mod` lo)
    | hi < target = fromIntegral (target `mod` hi)
    | otherwise = target

-- | Print the result of sending "/status".
dumpStatus :: Transport t => t -> IO ()
dumpStatus fd = serverStatus fd >>= mapM_ putStrLn

------------------------------------------------------------------------------
--
-- Randoms
--
------------------------------------------------------------------------------

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs rng  = randomRs rng `fmap` newStdGen

chooseOne :: RandomGen g => [a] -> g -> (a, g)
chooseOne xs g = (xs !! idx, g')
    where (idx, g') = randomR (0, length xs - 1) g

choose :: RandomGen g => [a] -> Int -> g -> [a]
choose xs num g = choose' [] xs num g
    where
      choose' :: RandomGen g => [a] -> [a] -> Int -> g -> [a]
      choose' xs' _  0 _ = xs'
      choose' xs' ys n g' = choose' (x:xs') ys' (n-1) g''
          where (idx,g'') = randomR (0,length ys - 1) g'
                x = ys !! idx
                (a,b) = splitAt idx ys
                ys' = a ++ tail b

choices :: RandomGen g => [a] -> g -> [a]
choices xs g = x:choices xs g'
    where (x,g') = chooseOne xs g

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g = (xs', g')
    where xs' = choose xs (length xs) g
          (_,g') = next g

shuffle' :: RandomGen g => [a] -> g -> [a]
shuffle' xs g = fst $ shuffle xs g

shuffleIO :: [a] -> IO [a]
shuffleIO = getStdRandom . shuffle

expRandomR :: (Floating a, RandomGen g, Random a) => (a, a) -> g -> (a, g)
expRandomR (lo,hi) g0 =  (exp x, g1)
    where (x, g1) = randomR (log lo, log hi) g0

expRandomRs :: (Floating a, RandomGen g, Random a) =>
               (a, a) -> g -> [a]
expRandomRs (lo,hi) = map exp . randomRs (log lo, log hi)

-- | Test for envelope shape. Play a sine tone with given shape.
envTest :: [UGen] -> IO ()
envTest ugs = audition oscil
    where
      oscil = out 0 $ sinOsc AR 880 0 * e
      e = envGen KR g 1 0 1 RemoveSynth ugs
      g = control KR "gate" 0.5

-- | Sustain the second to last value until gate set to 0.
envCoord' :: [(UGen, UGen)] -> UGen -> UGen -> EnvCurve -> [UGen]
envCoord' bp dur amp c =
    let l = map ((* amp) . snd) bp
        t = map (* dur) (d_dx (map fst bp))
    in undefined
    -- in  env l t (repeat c) 1 (-1)

-- d_dx :: (Num a) => [a] -> [a]
-- d_dx [] = []
-- d_dx [_] = []
-- d_dx [x,y] = [y - x]
-- d_dx (x:y:r) = y - x : d_dx (y:r)
-}
