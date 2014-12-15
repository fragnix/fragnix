{-# LINE 1 "./Data/Hashable/RandomSource.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Data/Hashable/RandomSource.hs" #-}
{-# LINE 1 "./Data/Hashable/RandomSource.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-# LANGUAGE Trustworthy #-}


module Data.Hashable.RandomSource
    (
      getRandomBytes
    , getRandomBytes_
    ) where

import Data.ByteString as B
import Data.ByteString.Internal (create)
import Foreign.C.Error (throwErrnoIfMinus1_)

import Foreign.C.Types (CInt(CInt))



import Foreign.Ptr (Ptr)

getRandomBytes :: Int -> IO ByteString
getRandomBytes nbytes
    | nbytes <= 0 = return B.empty
    | otherwise = create nbytes $ flip (getRandomBytes_ "getRandomBytes") nbytes

getRandomBytes_ :: String -> Ptr a -> Int -> IO ()
getRandomBytes_ what ptr nbytes = do
  throwErrnoIfMinus1_ what $ c_getRandomBytes ptr (fromIntegral nbytes)

foreign import ccall unsafe "hashable_getRandomBytes" c_getRandomBytes
    :: Ptr a -> CInt -> IO CInt
