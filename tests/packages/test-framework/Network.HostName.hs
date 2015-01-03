{-# LINE 1 "./Network/HostName.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Network/HostName.hs" #-}
{-# LINE 1 "./Network/HostName.hs" #-}
module Network.HostName (
    HostName, getHostName
  ) where

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array





























foreign import ccall unsafe "gethostname" gethostname :: CString -> CSize -> IO CInt

getHostName :: IO HostName
getHostName = allocaArray0 size $ \cstr -> do
        throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
        peekCString cstr
    where size = 256



type HostName = String
