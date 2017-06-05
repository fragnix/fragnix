{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/autogen/Paths_transformers_compat.hs" #-}




















































{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_transformers_compat (
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
version = Version [0,5,1,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/bin"
libdir     = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/transformers-compat-0.5.1.4-7muSvCUmtprGlIf415A9oc"
dynlibdir  = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/transformers-compat-0.5.1.4"
libexecdir = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/libexec"
sysconfdir = "/home/pschuster/Projects/haskell-modules/packages/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "transformers_compat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "transformers_compat_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "transformers_compat_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "transformers_compat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "transformers_compat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "transformers_compat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
