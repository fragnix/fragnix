{-# LINE 1 "Data/Streaming/Filesystem.hs" #-}
# 1 "Data/Streaming/Filesystem.hs"
# 1 "<command-line>"
# 8 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 8 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1



































































































































































# 8 "<command-line>" 2
# 1 "Data/Streaming/Filesystem.hs"
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Streaming functions for interacting with the filesystem.
module Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStream
    , closeDirStream
    , FileType (..)
    , getFileType
    ) where

import Data.Typeable (Typeable)

# 60 "Data/Streaming/Filesystem.hs"

import System.Posix.Directory (DirStream, openDirStream, closeDirStream)
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as PosixF
import Control.Exception (try, IOException)

readDirStream :: DirStream -> IO (Maybe FilePath)
readDirStream ds = do
    fp <- Posix.readDirStream ds
    case fp of
        "" -> return Nothing
        "." -> readDirStream ds
        ".." -> readDirStream ds
        _ -> return $ Just fp

getFileType :: FilePath -> IO FileType
getFileType fp = do
    s <- PosixF.getSymbolicLinkStatus fp
    case () of
        ()
            | PosixF.isRegularFile s -> return FTFile
            | PosixF.isDirectory s -> return FTDirectory
            | PosixF.isSymbolicLink s -> do
                es' <- try $ PosixF.getFileStatus fp
                case es' of
                    Left (_ :: IOException) -> return FTOther
                    Right s'
                        | PosixF.isRegularFile s' -> return FTFileSym
                        | PosixF.isDirectory s' -> return FTDirectorySym
                        | otherwise -> return FTOther
            | otherwise -> return FTOther



data FileType
    = FTFile
    | FTFileSym -- ^ symlink to file
    | FTDirectory
    | FTDirectorySym -- ^ symlink to a directory
    | FTOther
    deriving (Show, Read, Eq, Ord, Typeable)
