{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/HPACK/Builder.hs" #-}
module Network.HPACK.Builder where

newtype Builder a = Builder ([a] -> [a])

(<<) :: Builder a -> a -> Builder a
Builder b << entry = Builder $ b . (entry :)

empty :: Builder a
empty = Builder id

singleton :: a -> Builder a
singleton x = Builder (x :)

run :: Builder a -> [a]
run (Builder b) = b []
