module Paths_hsc3_wing (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/atsuro/.cabal/bin"
libdir     = "/home/atsuro/.cabal/lib/hsc3-wing-0.0.1/ghc-6.12.1"
datadir    = "/home/atsuro/.cabal/share/hsc3-wing-0.0.1"
libexecdir = "/home/atsuro/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "hsc3_wing_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "hsc3_wing_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "hsc3_wing_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "hsc3_wing_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
