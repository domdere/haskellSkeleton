module Paths_testMain (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/domdere/GitRepos/bhaskellSkeleton/testMain/bin"
libdir     = "/home/domdere/GitRepos/bhaskellSkeleton/testMain/lib/testMain-0.1.0.0/ghc-7.6.2"
datadir    = "/home/domdere/GitRepos/bhaskellSkeleton/testMain/share/testMain-0.1.0.0"
libexecdir = "/home/domdere/GitRepos/bhaskellSkeleton/testMain/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "testMain_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "testMain_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "testMain_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "testMain_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
