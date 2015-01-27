{-# LINE 1 "Data/Streaming/FileRead.hs" #-}
# 1 "Data/Streaming/FileRead.hs"
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
# 1 "Data/Streaming/FileRead.hs"
{-# LANGUAGE CPP #-}
-- | The standard @openFile@ call on Windows causing problematic file locking
-- in some cases. This module provides a cross-platform file reading API
-- without the file locking problems on Windows.
--
-- This module /always/ opens files in binary mode.
--
-- @readChunk@ will return an empty @ByteString@ on EOF.
module Data.Streaming.FileRead
    ( ReadHandle
    , openFile
    , closeFile
    , readChunk
    ) where







import qualified System.IO as IO
import qualified Data.ByteString as S
import Data.ByteString.Lazy.Internal (defaultChunkSize)

newtype ReadHandle = ReadHandle IO.Handle

openFile :: FilePath -> IO ReadHandle
openFile fp = ReadHandle `fmap` IO.openBinaryFile fp IO.ReadMode

closeFile :: ReadHandle -> IO ()
closeFile (ReadHandle h) = IO.hClose h

readChunk :: ReadHandle -> IO S.ByteString
readChunk (ReadHandle h) = S.hGetSome h defaultChunkSize

