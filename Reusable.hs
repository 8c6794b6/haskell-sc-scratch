----------------------------------------------------------------------
-- | Some maybe-reusable functions appeared while writing scratches
-- for parallel control of sequences. By the way, this file name is awful.
--
-- TODO: Move to hsc3-8c6794b6 library? ... might rename the library.
-- 

module Reusable where

import Data.List (nub)
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))

import Sound.SC3
import Sound.OpenSoundControl

import qualified Data.ByteString as B

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

-- | @flip async@.
async' :: OSC -> SendUDP OSC
async' = flip async

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
send' :: OSC -> SendUDP ()
send' = flip send

-- | Sends "/g_queryTree" and shows returning message.
queryRootNode :: Transport t => t -> IO OSC
queryRootNode fd = do
  send fd (Message "/g_queryTree" [Int 0, Int 1]) 
  wait fd "/g_queryTree.reply"

dumpRootNode :: Transport t => t -> IO ()
dumpRootNode fd = send fd (Message "/g_dumpTree" [Int 0, Int 1])