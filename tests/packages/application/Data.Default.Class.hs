{-# LANGUAGE Haskell98 #-}
{-# LINE 1 "Data/Default/Class.hs" #-}














































{-

Copyright (c) 2013 Lukas Mai

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the author nor the names of his contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY LUKAS MAI AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE CPP #-}


{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}

module Data.Default.Class (
-- | This module defines a class for types with a default value.
-- It also defines 'Default' instances for the types 'Int', 'Int8',
-- 'Int16', 'Int32', 'Int64', 'Word', 'Word8', 'Word16', 'Word32', 'Word64',
-- 'Integer', 'Float', 'Double', 'Ratio', 'Complex', 'CShort', 'CUShort',
-- 'CInt', 'CUInt', 'CLong', 'CULong', 'CLLong', 'CULLong', 'CPtrdiff',
-- 'CSize', 'CSigAtomic', 'CIntPtr', 'CUIntPtr', 'CIntMax', 'CUIntMax',
-- 'CClock', 'CTime', 'CUSeconds', 'CSUSeconds', 'CFloat', 'CDouble', '(->)',
-- 'IO', 'Maybe', '()', '[]', 'Ordering', 'Any', 'All', 'Last', 'First', 'Sum',
-- 'Product', 'Endo', 'Dual', and tuples.
    Default(..)
) where

import Data.Int
import Data.Word
import Data.Monoid
import Data.Ratio
import Data.Complex
import Foreign.C.Types

import GHC.Generics

class GDefault f where
    gdef :: f a

instance GDefault U1 where
    gdef = U1

instance (Default a) => GDefault (K1 i a) where
    gdef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
    gdef = gdef :*: gdef

instance (GDefault a) => GDefault (M1 i c a) where
    gdef = M1 gdef

-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a

    default def :: (Generic a, GDefault (Rep a)) => a
    def = to gdef

instance Default Int where def = 0
instance Default Int8 where def = 0
instance Default Int16 where def = 0
instance Default Int32 where def = 0
instance Default Int64 where def = 0
instance Default Word where def = 0
instance Default Word8 where def = 0
instance Default Word16 where def = 0
instance Default Word32 where def = 0
instance Default Word64 where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance (Integral a) => Default (Ratio a) where def = 0
instance (Default a, RealFloat a) => Default (Complex a) where def = def :+ def

instance Default CShort     where def = 0
instance Default CUShort    where def = 0
instance Default CInt       where def = 0
instance Default CUInt      where def = 0
instance Default CLong      where def = 0
instance Default CULong     where def = 0
instance Default CLLong     where def = 0
instance Default CULLong    where def = 0
instance Default CPtrdiff   where def = 0
instance Default CSize      where def = 0
instance Default CSigAtomic where def = 0
instance Default CIntPtr    where def = 0
instance Default CUIntPtr   where def = 0
instance Default CIntMax    where def = 0
instance Default CUIntMax   where def = 0
instance Default CClock     where def = 0
instance Default CTime      where def = 0
instance Default CUSeconds  where def = 0
instance Default CSUSeconds where def = 0
instance Default CFloat     where def = 0
instance Default CDouble    where def = 0

instance (Default r) => Default (e -> r) where def = const def
instance (Default a) => Default (IO a) where def = return def

instance Default (Maybe a) where def = Nothing

instance Default () where def = mempty
instance Default [a] where def = mempty
instance Default Ordering where def = mempty
instance Default Any where def = mempty
instance Default All where def = mempty
instance Default (Last a) where def = mempty
instance Default (First a) where def = mempty
instance (Num a) => Default (Sum a) where def = mempty
instance (Num a) => Default (Product a) where def = mempty
instance Default (Endo a) where def = mempty

instance (Default a) => Default (Dual a) where def = Dual def
instance (Default a, Default b) => Default (a, b) where def = (def, def)
instance (Default a, Default b, Default c) => Default (a, b, c) where def = (def, def, def)
instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where def = (def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where def = (def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f) => Default (a, b, c, d, e, f) where def = (def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where def = (def, def, def, def, def, def, def)
