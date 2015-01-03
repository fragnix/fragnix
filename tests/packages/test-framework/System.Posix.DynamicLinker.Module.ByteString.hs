{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Module/ByteString.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Module/ByteString.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/DynamicLinker/Module/ByteString.hs" #-}
{-# LINE 1 "System/Posix/DynamicLinker/Module/ByteString.hsc" #-}

{-# LINE 4 "System/Posix/DynamicLinker/Module/ByteString.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.DynamicLinker.Module.ByteString
-- Copyright   :  (c) Volker Stolz <vs@foldr.org> 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  vs@foldr.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- DLOpen support, old API
--  Derived from GModule.chs by M.Weber & M.Chakravarty which is part of c2hs
--  I left the API more or less the same, mostly the flags are different.
--
-----------------------------------------------------------------------------

module System.Posix.DynamicLinker.Module.ByteString (

--  Usage:
--  ******
--  
--  Let's assume you want to open a local shared library 'foo' (./libfoo.so)
--  offering a function
--    char * mogrify (char*,int)
--  and invoke str = mogrify("test",1):
-- 
--  type Fun = CString -> Int -> IO CString
--  foreign import dynamic unsafe fun__ :: FunPtr Fun -> Fun
-- 
--  withModule (Just ".") ("libfoo.so") [RTLD_NOW] $ \ mod -> do
--     funptr <- moduleSymbol mod "mogrify"
--     let fun = fun__ funptr
--     withCString "test" $ \ str -> do
--       strptr <- fun str 1
--       strstr <- peekCString strptr
--       ...

      Module
    , moduleOpen             -- :: String -> ModuleFlags -> IO Module
    , moduleSymbol           -- :: Source -> String -> IO (FunPtr a)
    , moduleClose            -- :: Module -> IO Bool
    , moduleError            -- :: IO String
    , withModule             -- :: Maybe String 
                             -- -> String 
	                     -- -> [ModuleFlags ]
			     -- -> (Module -> IO a) 
			     -- -> IO a
    , withModule_            -- :: Maybe String 
 			     -- -> String 
 			     -- -> [ModuleFlags] 
 			     -- -> (Module -> IO a) 
 			     -- -> IO ()
    )
where


{-# LINE 60 "System/Posix/DynamicLinker/Module/ByteString.hsc" #-}

import System.Posix.DynamicLinker.Module hiding (moduleOpen)
import System.Posix.DynamicLinker.Prim
import System.Posix.DynamicLinker.Common

import Foreign
import System.Posix.ByteString.FilePath

-- Opens a module (EXPORTED)
--

moduleOpen :: RawFilePath -> [RTLDFlags] -> IO Module
moduleOpen file flags = do
  modPtr <- withFilePath file $ \ modAddr -> c_dlopen modAddr (packRTLDFlags flags)
  if (modPtr == nullPtr)
      then moduleError >>= \ err -> ioError (userError ("dlopen: " ++ err))
      else return $ Module modPtr
