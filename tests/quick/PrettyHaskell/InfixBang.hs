{-# LANGUAGE BangPatterns #-}

{-# INLINE (!) #-}
-- | Returns the element of an immutable array at the specified index.
(!) :: (a -> b) -> a -> b
(!) arr i = arr i

