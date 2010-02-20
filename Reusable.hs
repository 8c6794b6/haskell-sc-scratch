{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
----------------------------------------------------------------------
-- | Some maybe-reusable functions appeared while writing scratches
-- for parallel control of sequences. By the way, this file name is awful.
--
-- TODO: Move to hsc3-8c6794b6 library? ... might rename the library.
-- 

module Reusable where

import Control.Arrow ((>>>))
import Data.List (nub,transpose)
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import System.Random (Random, RandomGen, randomR, randomRs, newStdGen)

import Sound.SC3
import Sound.OpenSoundControl

import qualified Data.ByteString.Lazy as B

-- | Environmental variable for synthdefs.
hsc3SynthdefDirEnvVar :: String
hsc3SynthdefDirEnvVar = "HSC3_SYNTHDEF_DIR"

-- | This function writes the synthdef file to directory specifyed by
-- environmental variable @HSC3_SYNTHDEF_DIR@. Though, to use the
-- synthdef, one must load the synthdef file explicitly, or execute
-- @withSC3 reloadSynthdef@ action.
writeSynthdef :: String -> UGen -> IO ()
writeSynthdef name ugen = do
  env <- getEnvironment
  let dir = maybe "./" id $ lookup hsc3SynthdefDirEnvVar env
      path = dir </> name <.> "scsyndef"
      contents = B.pack $ synthdef name ugen
  B.writeFile path contents

-- | Reload synthdef directory. If specified, path to the
-- @HSC3_SYNTHDEF_DIR@ would be used. Otherwise, current directory
-- would be used.
reloadSynthdef :: (Transport t) => t -> IO ()
reloadSynthdef fd = do
  env <- getEnvironment
  let dir = maybe "./" id $ lookup hsc3SynthdefDirEnvVar env
      path = dir </> ""
  send fd (d_loadDir path)

-- | Type synonym for sending osc message to scsynth.
type SendUDP a = UDP -> IO a

-- | Get rate, name, and default value of controls from given ugen.
getControls :: UGen -> [(Rate,String,Double)]
getControls = nub . getControls' 

getControls' :: UGen -> [(Rate,String,Double)]
getControls' ug =
    case ug of
      Constant _ -> []
      Control rate name def -> [(rate,name,def)]
      Primitive _ _ inputs _ _ _ -> concatMap getControls' inputs
      Proxy src _ -> getControls' src
      MCE ugs -> concatMap getControls' ugs
      MRG l r -> getControls' l ++ getControls' r

-- | @flip send@. With flipping the argument, one can write as below:
-- > withSC3 (send' some_osc_message)
send' :: Transport t => OSC -> (t -> IO ())
send' = flip send

-- | @flip async@.
async' :: Transport t => OSC -> (t -> IO OSC)
async' = flip async

-- | @flip wait@
wait' :: Transport t => String -> (t -> IO OSC)
wait' = flip wait

-- | Sends "/g_queryTree" and shows returning message.
queryTree :: Transport t => t -> IO OSC
queryTree fd = do
  send fd (Message "/g_queryTree" [Int 0, Int 1]) 
  wait fd "/g_queryTree.reply"

-- | Dumps root node and show in scsynth.
dumpTree :: Transport t => t -> IO ()
dumpTree = send' (Message "/g_dumpTree" [Int 0, Int 1])

-- | Sends a synthdef to server.
sendSynthdef :: Transport t => String -> UGen -> t -> IO OSC
sendSynthdef name ugen = async' $ d_recv $ synthdef name ugen

-- | Write a synthdef to file, then load it.
loadSynthdef :: Transport t => String -> UGen -> (t -> IO OSC)
loadSynthdef name ugen fd = do
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
-- > \fd ->
b_getn' :: Transport t => Int -> [(Int,Int)] -> (t -> IO OSC)
b_getn' id params = \fd -> send fd (b_getn id params) >> wait fd "/b_setn"

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
b_play bufnum loop = undefined



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

-- | Makes list of s_new osc messages from Map.
mkSNew :: String -> Int -> AddAction -> Int -> Map String [Double] -> [OSC]
mkSNew name nodeId addAction targetId =
  map (s_new name nodeId addAction targetId) . transposeWithKeys

-- | Variant of mkSNew with nodeId (-1) and AddToTail.
mkSNew' :: String -> Int -> Map String [Double] -> [OSC]
mkSNew' name targetId = mkSNew name (-1) AddToTail targetId

transposeWithKeys :: Map a [b] -> [[(a,b)]]
transposeWithKeys =
  transpose . M.elems . M.mapWithKey (\k a -> zip (repeat k) a)

-- Copied from current head of darcs repository.

-- List of asynchronous server commands.
async_cmds :: [String]
async_cmds = ["/d_recv", "/d_load", "/d_loadDir"
             ,"/b_alloc", "/b_allocRead", "/b_allocReadChannel"
             ,"/b_free", "/b_close"
             ,"/b_read", "/b_readChannel"
             ,"/b_write", "/b_zero"]

-- | Add a completion message to an existing asynchronous command.
withCM :: OSC -> OSC -> OSC
withCM (Message c xs) cm =
    if c `elem` async_cmds
    then let xs' = xs ++ [Blob (B.unpack (encodeOSC cm))]
         in Message c xs'
    else error ("withCM: not async: " ++ c)
withCM _ _ = error "withCM: not message"

-- | Print the result of sending "/status".
dumpStatus :: IO ()
dumpStatus = mapM_ putStrLn =<< withSC3 serverStatus

randomRIOs :: Random a => (a, a) -> IO [a]
randomRIOs range  = randomRs range `fmap` newStdGen

chooseOne :: RandomGen g => [a] -> g -> (a, g)
chooseOne xs g = (xs !! idx, g') 
    where (idx, g') = randomR (0, length xs - 1) g
    
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g = (xs', g') 
    where xs' = undefined
          g' = undefined