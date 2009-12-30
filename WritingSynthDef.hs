----------------------------------------------------------------------
-- | Scratchy module for writing SynthDef files.
-- 

module WritingSynthDef where

import Sound.SC3
import Sound.OpenSoundControl
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString as B

-- | Environmental variable for synthdefs.
hsc3SynthdefDirEnvVar :: String
hsc3SynthdefDirEnvVar = "HSC3_SYNTHDEF_DIR"

-- | This function writes the synthdef file to directory specifyed by
-- environmental variable @HSC3_SYNTHDEF_DIR@. Though, to use the
-- synthdef, one must load the synthdef file explicitly, or execute
-- @withSC3 reloadSynthdef@ action.
writeDefFile :: String -> UGen -> IO ()
writeDefFile name ugen = do
  env <- getEnvironment
  let dir = maybe "./" id $ lookup hsc3SynthdefDirEnvVar env
      path = dir </> name <.> "scsyndef"
      contents = B.pack $ synthdef name ugen
  B.writeFile path contents


-- | Reload synthdef directory. If specified, path to the
-- @HSC3_SYNTHDEF_DIR@ would be used. Otherwise, current directory
-- would be used.
reloadSynthDef :: (Transport t) => t -> IO ()
reloadSynthDef fd = do
  env <- getEnvironment
  let dir = maybe "./" id $ lookup hsc3SynthdefDirEnvVar env
      path = dir </> ""
  send fd (d_loadDir path)


-- | Simple ugen with mono channel sin oscillator.
simpleUGen :: UGen
simpleUGen = out 0 (sinOsc AR 440 0 * 0.5)