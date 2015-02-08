{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
module StandaloneDeriving where

import GHC.Generics

newtype Void = Void Void

deriving instance Generic Void
