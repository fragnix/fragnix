{-# LINE 1 "./Blaze/ByteString/Builder.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Blaze/ByteString/Builder.hs" #-}
{-# LINE 1 "./Blaze/ByteString/Builder.hs" #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Blaze.ByteString.Builder
-- Copyright   : (c) 2010 Jasper Van der Jeugt & Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- "Blaze.ByteString.Builder" is the main module, which you should import as a user
-- of the @blaze-builder@ library.
--
-- > import Blaze.ByteString.Builder
--
-- It provides you with a type 'Builder' that allows to efficiently construct
-- lazy bytestrings with a large average chunk size.
--
-- Intuitively, a 'Builder' denotes the construction of a part of a lazy
-- bytestring. Builders can either be created using one of the primitive
-- combinators in "Blaze.ByteString.Builder.Write" or by using one of the predefined
-- combinators for standard Haskell values (see the exposed modules of this
-- package).  Concatenation of builders is done using 'mappend' from the
-- 'Monoid' typeclass.
--
-- Here is a small example that serializes a list of strings using the UTF-8
-- encoding.
--
-- @ import "Blaze.ByteString.Builder.Char.Utf8"@
--
-- > strings :: [String]
-- > strings = replicate 10000 "Hello there!"
--
-- The function @'fromString'@ creates a 'Builder' denoting the UTF-8 encoded
-- argument. Hence, UTF-8 encoding and concatenating all @strings@ can be done
-- follows.
--
-- > concatenation :: Builder
-- > concatenation = mconcat $ map fromString strings
--
-- The function 'toLazyByteString'  can be used to execute a 'Builder' and
-- obtain the resulting lazy bytestring.
--
-- > result :: L.ByteString
-- > result = toLazyByteString concatenation
--
-- The @result@ is a lazy bytestring containing 10000 repetitions of the string
-- @\"Hello there!\"@ encoded using UTF-8. The corresponding 120000 bytes are
-- distributed among three chunks of 32kb and a last chunk of 6kb.
--
-- /A note on history./ This serialization library was inspired by the
-- @Data.Binary.Builder@ module provided by the @binary@ package. It was
-- originally developed with the specific needs of the @blaze-html@ package in
-- mind. Since then it has been restructured to serve as a drop-in replacement
-- for @Data.Binary.Builder@, which it improves upon both in speed as well as
-- expressivity.
-----------------------------------------------------------------------------

module Blaze.ByteString.Builder
    (
      -- * The 'Builder' type
      Builder

      -- * Creating builders
    , module Blaze.ByteString.Builder.Int
    , module Blaze.ByteString.Builder.Word
    , module Blaze.ByteString.Builder.ByteString
    , flush

      -- * Executing builders
    , toLazyByteString
    , toLazyByteStringWith
    , toByteString
    , toByteStringIO
    , toByteStringIOWith

    -- * 'Write's
    , Write
    , fromWrite
    , fromWriteSingleton
    , fromWriteList
    , writeToByteString

    -- ** Writing 'Storable's
    , writeStorable
    , fromStorable
    , fromStorables

    ) where

import Blaze.ByteString.Builder.Internal
import Blaze.ByteString.Builder.Int
import Blaze.ByteString.Builder.Word
import Blaze.ByteString.Builder.ByteString

