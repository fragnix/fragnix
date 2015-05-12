{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module F3977838970895265051 where
import qualified F9165819906499745633 as M (MVector)
import F5053685185079289079 (MVector)
import GHC.Types (Bool)
import F1690618865877934976 (MVector(MV_Bool))
import qualified F9165819906499745633 as M (basicLength)
import GHC.Base (($))
import qualified F9165819906499745633 as M (basicUnsafeSlice)
import qualified F9165819906499745633 as P (basicOverlaps)
import qualified F9165819906499745633 as M (basicOverlaps)
import qualified F9165819906499745633 as P (basicUnsafeNew)
import Control.Monad (liftM)
import qualified F9165819906499745633 as M (basicUnsafeNew)
import qualified F9165819906499745633 as P (basicUnsafeReplicate)
import qualified F9165819906499745633 as M (basicUnsafeReplicate)
import F4147502707560312710 (fromBool)
import qualified F9165819906499745633 as P (basicUnsafeRead)
import F8409505011132499003 (toBool)
import qualified F9165819906499745633 as M (basicUnsafeRead)
import qualified F9165819906499745633 as P (basicUnsafeWrite)
import qualified F9165819906499745633 as M (basicUnsafeWrite)
import qualified F9165819906499745633 as P (basicClear)
import qualified F9165819906499745633 as M (basicClear)
import qualified F9165819906499745633 as P (basicSet)
import qualified F9165819906499745633 as M (basicSet)
import qualified F9165819906499745633 as M (basicUnsafeCopy)
import qualified F9165819906499745633 as P (basicUnsafeMove)
import qualified F9165819906499745633 as M (basicUnsafeMove)
import qualified F9165819906499745633 as P (basicUnsafeGrow)
import qualified F9165819906499745633 as M (basicUnsafeGrow)

import Prelude (undefined)

instance M.MVector MVector Bool where
        {-# INLINE basicLength #-}
        
        {-# INLINE basicUnsafeSlice #-}
        
        {-# INLINE basicOverlaps #-}
        
        {-# INLINE basicUnsafeNew #-}
        
        {-# INLINE basicUnsafeReplicate #-}
        
        {-# INLINE basicUnsafeRead #-}
        
        {-# INLINE basicUnsafeWrite #-}
        
        {-# INLINE basicClear #-}
        
        {-# INLINE basicSet #-}
        
        {-# INLINE basicUnsafeCopy #-}
        
        {-# INLINE basicUnsafeGrow #-}
        basicLength (MV_Bool v) = undefined
        basicUnsafeSlice i n (MV_Bool v)
          = undefined
        basicOverlaps (MV_Bool v1) (MV_Bool v2) = undefined
        basicUnsafeNew n = undefined
        basicUnsafeReplicate n x
          = undefined
        basicUnsafeRead (MV_Bool v) i
          = undefined
        basicUnsafeWrite (MV_Bool v) i x
          = undefined
        basicClear (MV_Bool v) = undefined
        basicSet (MV_Bool v) x = undefined
        basicUnsafeCopy (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeCopy v1 v2
        basicUnsafeMove (MV_Bool v1) (MV_Bool v2) = M.basicUnsafeMove v1 v2
        basicUnsafeGrow (MV_Bool v) n
          = undefined