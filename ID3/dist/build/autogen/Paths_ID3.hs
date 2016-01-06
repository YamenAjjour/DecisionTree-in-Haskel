module Paths_ID3 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/yamen/.cabal/bin"
libdir     = "/home/yamen/.cabal/lib/ID3-0.0.1/ghc-7.6.3"
datadir    = "/home/yamen/.cabal/share/ID3-0.0.1"
libexecdir = "/home/yamen/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ID3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ID3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ID3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ID3_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
