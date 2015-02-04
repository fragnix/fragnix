{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/Env/ByteString.hs" #-}
{-# LINE 1 "System/Posix/Env/ByteString.hsc" #-}

{-# LINE 2 "System/Posix/Env/ByteString.hsc" #-}
{-# LANGUAGE Trustworthy #-}

{-# LINE 6 "System/Posix/Env/ByteString.hsc" #-}

{-# LINE 7 "System/Posix/Env/ByteString.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env.ByteString
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX environment support
--
-----------------------------------------------------------------------------

module System.Posix.Env.ByteString (
       -- * Environment Variables
        getEnv
        , getEnvDefault
        , getEnvironmentPrim
        , getEnvironment
        , putEnv
        , setEnv
       , unsetEnv

       -- * Program arguments
       , getArgs
) where


{-# LINE 37 "System/Posix/Env/ByteString.hsc" #-}

import Foreign
import Foreign.C
import Control.Monad    ( liftM )
import Data.Maybe       ( fromMaybe )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString (ByteString)

-- |'getEnv' looks up a variable in the environment.

getEnv :: ByteString -> IO (Maybe ByteString)
getEnv name = do
  litstring <- B.useAsCString name c_getenv
  if litstring /= nullPtr
     then liftM Just $ B.packCString litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault :: ByteString -> ByteString -> IO ByteString
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [ByteString]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  arr <- peekArray0 nullPtr c_environ
  mapM B.packCString arr

getCEnviron :: IO (Ptr CString)

{-# LINE 81 "System/Posix/Env/ByteString.hsc" #-}
getCEnviron = peek c_environ_p

foreign import ccall unsafe "&environ"
   c_environ_p :: Ptr (Ptr CString)

{-# LINE 86 "System/Posix/Env/ByteString.hsc" #-}

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(ByteString,ByteString)]
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(BC.break ((==) '='))) env
 where
   dropEq (x,y)
      | BC.head y == '=' = (x,B.tail y)
      | otherwise       = error $ "getEnvironment: insane variable " ++ BC.unpack x

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: ByteString -> IO ()

{-# LINE 104 "System/Posix/Env/ByteString.hsc" #-}

unsetEnv name = B.useAsCString name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

foreign import ccall unsafe "__hsunix_unsetenv"
   c_unsetenv :: CString -> IO CInt

{-# LINE 113 "System/Posix/Env/ByteString.hsc" #-}

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: ByteString -> IO ()
putEnv keyvalue = B.useAsCString keyvalue $ \s ->
  throwErrnoIfMinus1_ "putenv" (c_putenv s)

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv :: ByteString -> ByteString -> Bool {-overwrite-} -> IO ()

{-# LINE 133 "System/Posix/Env/ByteString.hsc" #-}
setEnv key value ovrwrt = do
  B.useAsCString key $ \ keyP ->
    B.useAsCString value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum ovrwrt))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt

{-# LINE 149 "System/Posix/Env/ByteString.hsc" #-}

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (not including the program name), as 'ByteString's.
--
-- Unlike 'System.Environment.getArgs', this function does no Unicode
-- decoding of the arguments; you get the exact bytes that were passed
-- to the program by the OS.  To interpret the arguments as text, some
-- Unicode decoding should be applied.
--
getArgs :: IO [ByteString]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= mapM B.packCString

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
