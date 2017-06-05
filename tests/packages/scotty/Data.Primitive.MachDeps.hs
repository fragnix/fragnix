{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/Primitive/MachDeps.hs" #-}




















































{-# LANGUAGE CPP, MagicHash #-}
-- |
-- Module      : Data.Primitive.MachDeps
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Machine-dependent constants
--

module Data.Primitive.MachDeps where



































































































































































































































































































































































import GHC.Prim

sIZEOF_CHAR,
 aLIGNMENT_CHAR,

 sIZEOF_INT,
 aLIGNMENT_INT,

 sIZEOF_WORD,
 aLIGNMENT_WORD,

 sIZEOF_DOUBLE,
 aLIGNMENT_DOUBLE,

 sIZEOF_FLOAT,
 aLIGNMENT_FLOAT,

 sIZEOF_PTR,
 aLIGNMENT_PTR,

 sIZEOF_FUNPTR,
 aLIGNMENT_FUNPTR,

 sIZEOF_STABLEPTR,
 aLIGNMENT_STABLEPTR,

 sIZEOF_INT8,
 aLIGNMENT_INT8,

 sIZEOF_WORD8,
 aLIGNMENT_WORD8,

 sIZEOF_INT16,
 aLIGNMENT_INT16,

 sIZEOF_WORD16,
 aLIGNMENT_WORD16,

 sIZEOF_INT32,
 aLIGNMENT_INT32,

 sIZEOF_WORD32,
 aLIGNMENT_WORD32,

 sIZEOF_INT64,
 aLIGNMENT_INT64,

 sIZEOF_WORD64,
 aLIGNMENT_WORD64 :: Int


sIZEOF_CHAR = 4
aLIGNMENT_CHAR = 4

sIZEOF_INT = 8
aLIGNMENT_INT = 8

sIZEOF_WORD = 8
aLIGNMENT_WORD = 8

sIZEOF_DOUBLE = 8
aLIGNMENT_DOUBLE = 8

sIZEOF_FLOAT = 4
aLIGNMENT_FLOAT = 4

sIZEOF_PTR = 8
aLIGNMENT_PTR = 8

sIZEOF_FUNPTR = 8
aLIGNMENT_FUNPTR = 8

sIZEOF_STABLEPTR = 8
aLIGNMENT_STABLEPTR = 8

sIZEOF_INT8 = 1
aLIGNMENT_INT8 = 1

sIZEOF_WORD8 = 1
aLIGNMENT_WORD8 = 1

sIZEOF_INT16 = 2
aLIGNMENT_INT16 = 2

sIZEOF_WORD16 = 2
aLIGNMENT_WORD16 = 2

sIZEOF_INT32 = 4
aLIGNMENT_INT32 = 4

sIZEOF_WORD32 = 4
aLIGNMENT_WORD32 = 4

sIZEOF_INT64 = 8
aLIGNMENT_INT64 = 8

sIZEOF_WORD64 = 8
aLIGNMENT_WORD64 = 8

type Word64_# = Word#
type Int64_# = Int#

