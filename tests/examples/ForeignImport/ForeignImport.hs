module ForeignImport where
import GHC.Ptr (Ptr)
import GHC.Word (Word8)
import System.IO (IO)
import Foreign.C.Types (CInt(..),CSize(..))

foreign import ccall unsafe "string.h memcpy" c_memcpy ::
               Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)
