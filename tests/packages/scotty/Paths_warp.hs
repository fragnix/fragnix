{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/autogen/Paths_warp.hs" #-}
module Paths_warp (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [3,0,7], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pschuster/Projects/demo/bin"
libdir     = "/home/pschuster/Projects/demo/lib/x86_64-linux-ghc-7.8.3/warp-3.0.7"
datadir    = "/home/pschuster/Projects/demo/share/x86_64-linux-ghc-7.8.3/warp-3.0.7"
libexecdir = "/home/pschuster/Projects/demo/libexec"
sysconfdir = "/home/pschuster/Projects/demo/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "warp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
