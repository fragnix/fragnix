{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples,
  UnliftedFFITypes, DeriveDataTypeable, MultiParamTypeClasses,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F7160398609187403042 where
import Data.Typeable.Internal (Typeable)
import Data.Data (Data)
import {-# SOURCE #-} F9115654309439442691 (MutableByteArray)
import Data.Data (toConstr)
import GHC.Err (error)
import Data.Data (gunfold)
import Data.Data (dataTypeOf)
import Data.Data (mkNoRepType)

instance Typeable s => Data (MutableByteArray s)