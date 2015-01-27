{-# LINE 1 "Data/Streaming/Filesystem.hs" #-}

































































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
