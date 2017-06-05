{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "src/Data/Either/Compat.hs" #-}

















































{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Either.Compat (
  module Base
, isLeft
, isRight
, fromLeft
, fromRight
) where
import Data.Either as Base


-- | Return the contents of a 'Left'-value or a default value otherwise.
--
-- /Since: 4.10.0.0/
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromLeft 1 (Left 3)
-- 3
-- >>> fromLeft 1 (Right "foo")
-- 1
--
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

-- | Return the contents of a 'Right'-value or a default value otherwise.
--
-- /Since: 4.10.0.0/
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromRight 1 (Right 3)
-- 3
-- >>> fromRight 1 (Left "foo")
-- 1
--
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
