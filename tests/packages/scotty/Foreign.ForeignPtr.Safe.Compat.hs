{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Foreign/ForeignPtr/Safe/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.ForeignPtr.Safe.Compat (
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
        , FinalizerEnvPtr
        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
        , withForeignPtr

        , finalizeForeignPtr

        -- ** Low-level operations
        , touchForeignPtr
        , castForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
    ) where

import Foreign.ForeignPtr
