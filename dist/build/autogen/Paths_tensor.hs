module Paths_tensor (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/nate/.cabal/bin"
libdir     = "/home/nate/.cabal/lib/tensor-0.1.0.0/ghc-7.6.3"
datadir    = "/home/nate/.cabal/share/tensor-0.1.0.0"
libexecdir = "/home/nate/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "tensor_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tensor_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tensor_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tensor_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
