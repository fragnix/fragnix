{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module MVectorData where
import GHC.Types (Int)
import Data.Typeable.Internal (Typeable)

data MVector s a = MVector {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                 deriving Typeable