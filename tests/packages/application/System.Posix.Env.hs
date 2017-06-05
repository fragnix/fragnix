{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-261cd265/build/System/Posix/Env.hs" #-}
{-# LINE 1 "System/Posix/Env.hsc" #-}
{-# LANGUAGE CApiFFI #-}
{-# LINE 2 "System/Posix/Env.hsc" #-}

{-# LINE 3 "System/Posix/Env.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 7 "System/Posix/Env.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Env
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

module System.Posix.Env (
      getEnv
    , getEnvDefault
    , getEnvironmentPrim
    , getEnvironment
    , setEnvironment
    , putEnv
    , setEnv
    , unsetEnv
    , clearEnv
) where


{-# LINE 34 "System/Posix/Env.hsc" #-}

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Posix.Internals


{-# LINE 50 "System/Posix/Env.hsc" #-}

-- |'getEnv' looks up a variable in the environment.

getEnv ::
  String            {- ^ variable name  -} ->
  IO (Maybe String) {- ^ variable value -}
getEnv name = do
  litstring <- withFilePath name c_getenv
  if litstring /= nullPtr
     then liftM Just $ peekFilePath litstring
     else return Nothing

-- |'getEnvDefault' is a wrapper around 'getEnv' where the
-- programmer can specify a fallback if the variable is not found
-- in the environment.

getEnvDefault ::
  String    {- ^ variable name                    -} ->
  String    {- ^ fallback value                   -} ->
  IO String {- ^ variable value or fallback value -}
getEnvDefault name fallback = liftM (fromMaybe fallback) (getEnv name)

foreign import ccall unsafe "getenv"
   c_getenv :: CString -> IO CString

getEnvironmentPrim :: IO [String]
getEnvironmentPrim = do
  c_environ <- getCEnviron
  -- environ can be NULL
  if c_environ == nullPtr
    then return []
    else do
      arr <- peekArray0 nullPtr c_environ
      mapM peekFilePath arr

getCEnviron :: IO (Ptr CString)

{-# LINE 94 "System/Posix/Env.hsc" #-}
getCEnviron = peek c_environ_p
foreign import ccall unsafe "&environ"
   c_environ_p :: Ptr (Ptr CString)

{-# LINE 98 "System/Posix/Env.hsc" #-}

-- |'getEnvironment' retrieves the entire environment as a
-- list of @(key,value)@ pairs.

getEnvironment :: IO [(String,String)] {- ^ @[(key,value)]@ -}
getEnvironment = do
  env <- getEnvironmentPrim
  return $ map (dropEq.(break ((==) '='))) env
 where
   dropEq (x,'=':ys) = (x,ys)
   dropEq (x,_)      = error $ "getEnvironment: insane variable " ++ x

-- |'setEnvironment' resets the entire environment to the given list of
-- @(key,value)@ pairs.

setEnvironment ::
  [(String,String)] {- ^ @[(key,value)]@ -} ->
  IO ()
setEnvironment env = do
  clearEnv
  forM_ env $ \(key,value) ->
    setEnv key value True {-overwrite-}

-- |The 'unsetEnv' function deletes all instances of the variable name
-- from the environment.

unsetEnv :: String {- ^ variable name -} -> IO ()

{-# LINE 126 "System/Posix/Env.hsc" #-}

{-# LINE 127 "System/Posix/Env.hsc" #-}
unsetEnv name = withFilePath name $ \ s ->
  throwErrnoIfMinus1_ "unsetenv" (c_unsetenv s)

-- POSIX.1-2001 compliant unsetenv(3)
foreign import capi unsafe "HsUnix.h unsetenv"
   c_unsetenv :: CString -> IO CInt

{-# LINE 140 "System/Posix/Env.hsc" #-}

{-# LINE 143 "System/Posix/Env.hsc" #-}

-- |'putEnv' function takes an argument of the form @name=value@
-- and is equivalent to @setEnv(key,value,True{-overwrite-})@.

putEnv :: String {- ^ "key=value" -} -> IO ()
putEnv keyvalue = do s <- newFilePath keyvalue
                     -- Do not free `s` after calling putenv.
                     -- According to SUSv2, the string passed to putenv
                     -- becomes part of the environment. #7342
                     throwErrnoIfMinus1_ "putenv" (c_putenv s)

{-# LINE 158 "System/Posix/Env.hsc" #-}

foreign import ccall unsafe "putenv"
   c_putenv :: CString -> IO CInt

{- |The 'setEnv' function inserts or resets the environment variable name in
     the current environment list.  If the variable @name@ does not exist in the
     list, it is inserted with the given value.  If the variable does exist,
     the argument @overwrite@ is tested; if @overwrite@ is @False@, the variable is
     not reset, otherwise it is reset to the given value.
-}

setEnv ::
  String {- ^ variable name  -} ->
  String {- ^ variable value -} ->
  Bool   {- ^ overwrite      -} ->
  IO ()

{-# LINE 175 "System/Posix/Env.hsc" #-}
setEnv key value ovrwrt = do
  withFilePath key $ \ keyP ->
    withFilePath value $ \ valueP ->
      throwErrnoIfMinus1_ "setenv" $
        c_setenv keyP valueP (fromIntegral (fromEnum ovrwrt))

foreign import ccall unsafe "setenv"
   c_setenv :: CString -> CString -> CInt -> IO CInt

{-# LINE 191 "System/Posix/Env.hsc" #-}

-- |The 'clearEnv' function clears the environment of all name-value pairs.
clearEnv :: IO ()

{-# LINE 195 "System/Posix/Env.hsc" #-}
clearEnv = void c_clearenv

foreign import ccall unsafe "clearenv"
  c_clearenv :: IO Int

{-# LINE 206 "System/Posix/Env.hsc" #-}
