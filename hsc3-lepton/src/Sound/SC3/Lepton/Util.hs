{-# LANGUAGE ExistentialQuantification #-} 
{-# LANGUAGE Rank2Types #-}
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

import Control.Arrow ((>>>))
import Data.List (nub)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import System.Random 
  ( Random
  , RandomGen
  , getStdRandom
  , next
  , randomR
  , randomRs
  , newStdGen )
import qualified Data.ByteString.Lazy as B

import Sound.SC3
import Sound.OpenSoundControl

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
  let dir = maybe "./" id $ lookup scSynthdefPath envir
      path = dir </> name <.> "scsyndef"
      contents = B.pack $ synthdef name ugen
  B.writeFile path contents

-- | Reload synthdef directory. If specified, path to the
-- @HSC3_SYNTHDEF_DIR@ would be used. Otherwise, current directory
-- would be used.
reloadSynthdef :: (Transport t) => t -> IO OSC
reloadSynthdef fd = do
  envir <- getEnvironment
  let dir = maybe "./" id $ lookup scSynthdefPath envir
      path = dir </> ""
  async fd (d_loadDir path)

-- | Type synonym for sending osc message to scsynth.
type SendUDP a = UDP -> IO a

-- | Get rate, name, and default value of controls from given ugen.
getControls :: UGen -> [(Rate,String,Double,Bool)]
getControls = nub . getControls'

getControls' :: UGen -> [(Rate,String,Double,Bool)]
getControls' ug =
    case ug of
      Constant _ -> []
      Control rate name def trg -> [(rate,name,def,trg)]
      Primitive _ _ inputs _ _ _ -> concatMap getControls' inputs
      Proxy src _ -> getControls' src
      MCE ugs -> concatMap getControls' ugs
      MRG l r -> getControls' l ++ getControls' r

-- | Datatype for representing '/b_info' message returned by sending
-- '/b_query'.
data BufInfo = BufInfo {
      bufNumber :: Int,
      bufNumFrames :: Int,
      bufNumChannels :: Int,
      bufSampleRate :: Double 
    } deriving (Eq, Show)
                         
-- | Send /b_query and returns BufInfo.
getBufInfo :: Int -> IO BufInfo
getBufInfo bufId = do
  msg <- withSC3 (\fd -> send fd (b_query [bufId]) >> 
                         wait fd "/b_info")
  case msg of
    Message "/b_info" [Int bid, Int nf,Int nc,Float sr] -> 
        return $ BufInfo bid nf nc sr
    _ -> error "Not a /b_info message"

-- | @flip send@. With flipping the argument, one can write as below:
-- 
-- > withSC3 (send' some_osc_message)
send' :: Transport t => OSC -> (t -> IO ())
send' = flip send

-- | @flip async@.
async' :: Transport t => OSC -> (t -> IO OSC)
async' = flip async

-- | @flip wait@
wait' :: Transport t => String -> (t -> IO OSC)
wait' = flip wait

queryRootNode :: (Transport t) => t -> IO OSC
queryRootNode fd = do
  send fd (Message "/g_queryTree" [Int 0, Int 1])
  wait fd "/g_queryTree.reply"

-- | Sends "/g_queryTree" and shows returning message.
queryTree :: Int -> OSC
queryTree n = Message "/g_queryTree" [Int n, Int 1]

-- | Sends registration for notify, then query the nodes and shows
-- returing message.
queryNode :: Transport t => Int -> (t -> IO OSC)
queryNode n = \fd -> do
  async fd (notify True)
  send fd (n_query [n])
  wait fd "/n_info"                

-- | Dumps root node and show in scsynth.
dumpTree :: Transport t => t -> IO ()
dumpTree = send' (Message "/g_dumpTree" [Int 0, Int 1])

-- | Sends a synthdef to server.
sendSynthdef :: Transport t => String -> UGen -> t -> IO OSC
sendSynthdef name ugen = \t -> async t $ d_recv $ synthdef name ugen

-- | Write a synthdef to file, then load it.
loadSynthdef :: Transport t => String -> UGen -> (t -> IO OSC)
loadSynthdef name ugen _ = do
  writeSynthdef name ugen
  withSC3 $ sendSynthdef name ugen

-- | Sends a "/s_new" message, with default parameters and autogenerated
-- node Id, adding to tail of default group. Returns the autogenarated
-- node id.
s_new' :: Transport t => String -> [(String,Double)] -> t -> IO Int
s_new' name ps fd = do
  send fd $ s_new name (-1) AddToTail 1 ps
  send fd $ s_get (-1) []
  (Message _ (Int i:_)) <- wait fd "/n_set"
  return i

-- | Sends @/b_get@ message and wait until it gets @/b_set@ message.
b_get' :: Transport t => Int -> [Int] -> (t -> IO OSC)
b_get' bId is = \fd -> send fd (b_get bId is) >> wait fd "/b_set"

-- | Sends @/b_getn@ message and wait until it gets @/b_setn@.
-- 
-- > \fd ->
b_getn' :: Transport t => Int -> [(Int,Int)] -> (t -> IO OSC)
b_getn' bid params = \fd -> send fd (b_getn bid params) >> wait fd "/b_setn"

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


-- | Sends @/c_get@ message and wait until it gets @/c_set@.
c_get' :: Transport t => [Int] -> (t -> IO OSC)
c_get' ids = \fd -> send fd  (c_get ids) >> wait fd "/c_set"

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = (>>>)

(>>=*) :: (Functor f) => f a -> (a -> b) -> f b
(>>=*) = flip fmap

(>>.) :: Functor f => f a -> (a -> b) -> f b
(>>.) = flip fmap

-- | @a \@> b = b a@
(@>) :: a -> (a -> b) -> b
a @> b = b a

infixr 2 @>

-- | Returns Control ugen.
(@=) :: String -> Double -> UGen
name @= val = control kr name val

-- | Returns Control ugen, with lag.
(@~) :: String -> Double -> UGen -> UGen
a @~ b = \c -> lag (a @= b) c

-- | Fit in specified range with using @mod@.
fitInRange :: Integral a => a -> a -> a -> a
fitInRange lo hi target
    | target < lo = fromIntegral (target `mod` lo)
    | hi < target = fromIntegral (target `mod` hi)
    | otherwise = target

-- | Print the result of sending "/status".
dumpStatus :: Transport t => t -> IO ()
dumpStatus fd = serverStatus fd >>= mapM_ putStrLn

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs range  = randomRs range `fmap` newStdGen

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
      oscil = out 0 $ sinOsc ar 880 0 * e
      e = envGen kr g 1 0 1 RemoveSynth ugs 
      g = control kr "gate" 0.5

-- | Sustain the second to last value until gate set to 0.
envCoord' :: [(UGen, UGen)] -> UGen -> UGen -> EnvCurve -> [UGen]
envCoord' bp dur amp c = 
    let l = map ((* amp) . snd) bp
        t = map (* dur) (d_dx (map fst bp))
    in  env l t (repeat c) 1 (-1)

d_dx :: (Num a) => [a] -> [a]
d_dx [] = []
d_dx [_] = []
d_dx [x,y] = [y - x]
d_dx (x:y:r) = y - x : d_dx (y:r)
