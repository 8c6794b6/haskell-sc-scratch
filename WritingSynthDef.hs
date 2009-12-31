----------------------------------------------------------------------
-- | Scratchy module for writing SynthDef files.
-- 

module WritingSynthDef where

import Sound.SC3
import Sound.OpenSoundControl
import System.Environment (getEnvironment)
import System.FilePath ((</>), (<.>))
import System.Random
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

-- | Simple ugen with mono channel sin oscillator.
simpleUGen :: UGen
simpleUGen = out 0 (sinOsc AR 440 0 * 0.5)

-- | UGen with its params determined randomly.
simpleRandomUGen :: IO UGen
simpleRandomUGen = do
   freqs <- fmap (take 5) (newStdGen >>= return . randomRs (50,1200))
   let ugens = out 0 (sinOsc AR (mce freqs) 0 * 0.2)
   return ugens

-- | UGen with taking parameters.
simpleParamUGen :: UGen
simpleParamUGen = out channel oscillator where
    channel = Control KR "out" 0
    oscillator = sinOsc AR freq 0 * amp
    amp = Control KR "amp" 0.3
    freq = Control KR "freq" 440

-- | Sends new simpleParam UGen with specifying parameters.
sendSimpleParam :: Int -> Double -> Double -> Double -> IO ()
sendSimpleParam nodeId channel amp freq = do
    writeSynthdef "simpleParam" simpleParamUGen
    withSC3 $ \fd -> send fd (s_new "simpleParam" 

