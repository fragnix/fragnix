{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable,
  MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables,
  NondecreasingIndentation, ExplicitForAll, PatternGuards #-}
module F8599798322733369681 where
import GHC.Types (Int)
import Data.Typeable.Internal (Typeable)

data MVector s a = MVector {-# UNPACK #-} !Int {-# UNPACK #-} !Int Int
type role MVector phantom phantom
