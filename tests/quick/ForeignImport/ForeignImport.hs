module ForeignImport where
import GHC.Ptr (Ptr)
import GHC.Word (Word8)
import System.IO (IO)
import Foreign.C.Types (CInt(..),CSize(..))
import System.Posix.Types (Fd(..),COff(..))
import Data.Int (Int64)

foreign import ccall unsafe "string.h memcpy" c_memcpy ::
               Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "sendfile" c_sendfile ::
               Fd -> Fd -> Ptr COff -> CSize -> IO (Int64)
