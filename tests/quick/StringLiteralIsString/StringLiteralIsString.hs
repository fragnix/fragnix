{-# LANGUAGE OverloadedStrings #-}
module StringLiteralIsString where

import Data.String (IsString(fromString))

data CaseInsensitive a = CaseInsensitive a

newtype T = T String

instance IsString T where
    fromString = T

class FoldCase a where
    mapToLower :: a -> a

instance FoldCase T where
    mapToLower a = a

instance (IsString a, FoldCase a) => IsString (CaseInsensitive a) where
  fromString = CaseInsensitive . mapToLower . fromString

hUserAgent :: CaseInsensitive T
hUserAgent = "User-Agent"

