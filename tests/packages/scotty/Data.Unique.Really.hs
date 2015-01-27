{-# LINE 1 "src/Data/Unique/Really.hs" #-}
# 1 "src/Data/Unique/Really.hs"
# 1 "<command-line>"
# 9 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 9 "<command-line>" 2
# 1 "./dist/dist-sandbox-d76e0d17/build/autogen/cabal_macros.h" 1




































































































# 9 "<command-line>" 2
# 1 "src/Data/Unique/Really.hs"
-- | An abstract interface to a unique symbol generator.
module Data.Unique.Really (
    Unique, newUnique, hashUnique,
    ) where

import Control.Applicative
import Data.Hashable



import Control.Exception (evaluate)
import qualified Data.Unique
import System.Mem.StableName

-- | An abstract unique value.
-- Values of type 'Unique' may be compared for equality
-- and hashed into Int.
--
-- Note: Unlike the symbols from "Data.Unique", the symbols from this
-- module do not become equal after reloads in the GHC interpreter!
newtype Unique = Unique (StableName Data.Unique.Unique) deriving (Eq)

newUnique = do
    x <- Data.Unique.newUnique
    _ <- evaluate x
    Unique <$> makeStableName x

hashUnique (Unique s) = hashStableName s

# 54 "src/Data/Unique/Really.hs"

-- | Creates a new object of type 'Unique'.
-- The value returned will not compare equal to any other
-- value of type 'Unique' returned by previous calls to 'newUnique'.
-- There is no limit on the number of times you may call this function.
newUnique  :: IO Unique

-- | Hashes a 'Unique' into an 'Int'.
-- Two Uniques may hash to the same value, although in practice this is unlikely.
-- The 'Int' returned makes a good hash key.
hashUnique :: Unique -> Int

instance Hashable Unique where hashWithSalt s = hashWithSalt s . hashUnique
