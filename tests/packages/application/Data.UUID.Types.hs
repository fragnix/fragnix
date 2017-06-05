{-# LANGUAGE Haskell98, DeriveDataTypeable #-}
{-# LINE 1 "Data/UUID/Types.hs" #-}
{- |
Module      : Data.UUID.Types
Copyright   : (c) 2008,2012 Antoine Latter

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable


This library is useful for comparing, parsing and
printing Universally Unique Identifiers.
See <http://en.wikipedia.org/wiki/UUID> for the general idea.
See <http://tools.ietf.org/html/rfc4122> for the specification.

-}
module Data.UUID.Types(UUID
                ,toString
                ,fromString
                ,toText
                ,fromText
                ,toASCIIBytes
                ,fromASCIIBytes
                ,toLazyASCIIBytes
                ,fromLazyASCIIBytes
                ,toByteString
                ,fromByteString
                ,toWords
                ,fromWords
                ,null
                ,nil
                ) where

import Prelude () -- we need to hide Prelude.null
import Data.UUID.Types.Internal

-- Everything is really implemented in Data.UUID.Types.Internal, but I
-- don't want to export the constructors out of the package.
