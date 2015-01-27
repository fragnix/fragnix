{-# LINE 1 "src/Data/Generics/Instances.hs" #-}









































{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Instances
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Data)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- contains thirteen 'Data' instances which are considered dubious (either
-- because the types are abstract or just not meant to be traversed).
-- Instances in this module might change or disappear in future releases
-- of this package.
--
-- (This module does not export anything. It really just defines instances.)
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Generics.Instances () where

------------------------------------------------------------------------------

import Data.Data

import GHC.IO.Handle         -- So we can give Data instance for Handle
import GHC.Stable            -- So we can give Data instance for StablePtr
import GHC.ST                -- So we can give Data instance for ST
import GHC.Conc              -- So we can give Data instance for TVar
import Data.IORef            -- So we can give Data instance for IORef
import Control.Concurrent    -- So we can give Data instance for MVar

-- Version compatibility issues caused by #2760
myMkNoRepType :: String -> DataType
myMkNoRepType = mkNoRepType


------------------------------------------------------------------------------
--
--      Instances of the Data class for Prelude-like types.
--      We define top-level definitions for representations.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Instances of abstract datatypes (6)
------------------------------------------------------------------------------

instance Data TypeRep where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "Data.Typeable.TypeRep"


------------------------------------------------------------------------------

instance Data TyCon where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "Data.Typeable.TyCon"


------------------------------------------------------------------------------
deriving instance Typeable DataType

instance Data DataType where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "Data.Generics.Basics.DataType"


------------------------------------------------------------------------------

instance Data Handle where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.IOBase.Handle"


------------------------------------------------------------------------------

instance Typeable a => Data (StablePtr a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.Stable.StablePtr"


------------------------------------------------------------------------------

instance Data ThreadId where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.Conc.ThreadId"


------------------------------------------------------------------------------
-- Dubious instances (7)
------------------------------------------------------------------------------

instance Typeable a => Data (TVar a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.Conc.TVar"


------------------------------------------------------------------------------

instance Typeable a => Data (MVar a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.Conc.MVar"


------------------------------------------------------------------------------

instance Typeable a => Data (STM a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.Conc.STM"


------------------------------------------------------------------------------

instance (Typeable s, Typeable a) => Data (ST s a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.ST.ST"


------------------------------------------------------------------------------

instance Typeable a => Data (IORef a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.IOBase.IORef"


------------------------------------------------------------------------------

instance Typeable a => Data (IO a) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "GHC.IOBase.IO"

------------------------------------------------------------------------------

--
-- A last resort for functions
--

instance (Data a, Data b) => Data (a -> b) where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = myMkNoRepType "Prelude.(->)"
  dataCast2 f  = gcast2 f

