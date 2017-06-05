{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/autogen/Paths_warp.hs" #-}






















































































































{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_warp (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a

catchIO = Exception.catch

version :: Version
version = Version [3,2,12] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/bin"
libdir     = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/warp-3.2.12-FDdbjPuqDjPHngX2arlxz1"
dynlibdir  = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/warp-3.2.12"
libexecdir = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/libexec"
sysconfdir = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warp_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warp_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
