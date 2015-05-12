{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module MVectorClassInstance where
import qualified MVectorClass as M (MVector)
import MVectorFamily (MVector)
import GHC.Types (Bool)
import MVectorFamilyInstance (MVector(MV_Bool))
import qualified MVectorClass as M (basicLength)

instance M.MVector MVector Bool where
        {-# INLINE basicLength #-}
        basicLength (MV_Bool v) = M.basicLength v
