{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Utils/Containers/Internal/PtrEquality.hs" #-}























































{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Really unsafe pointer equality
module Utils.Containers.Internal.PtrEquality (ptrEq, hetPtrEq) where

import GHC.Exts ( reallyUnsafePtrEquality# )
import Unsafe.Coerce ( unsafeCoerce )
import GHC.Exts ( isTrue# )

-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq :: a -> a -> Bool

-- | Checks if two pointers are equal, without requiring
-- them to have the same type. The values should be forced
-- to at least WHNF before comparison to get moderately
-- reliable results.
hetPtrEq :: a -> b -> Bool

ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
hetPtrEq x y = isTrue# (unsafeCoerce reallyUnsafePtrEquality# x y)


{-# INLINE ptrEq #-}
{-# INLINE hetPtrEq #-}

infix 4 `ptrEq`
infix 4 `hetPtrEq`
