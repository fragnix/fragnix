{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "dist/dist-sandbox-d76e0d17/build/System/Posix/Signals/Exts.hs" #-}













































{-# LINE 1 "System/Posix/Signals/Exts.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 3 "System/Posix/Signals/Exts.hsc" #-}
{-# LANGUAGE Safe #-}

{-# LINE 5 "System/Posix/Signals/Exts.hsc" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX, includes Linuxisms/BSDisms)
--
-- non-POSIX signal support commonly available
--
-----------------------------------------------------------------------------


{-# LINE 21 "System/Posix/Signals/Exts.hsc" #-}


























































































































































































































































































{-# LINE 24 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 25 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 26 "System/Posix/Signals/Exts.hsc" #-}

module System.Posix.Signals.Exts (
  module System.Posix.Signals
  , sigINFO
  , sigWINCH
  , infoEvent
  , windowChange
  ) where

import Foreign.C
import System.Posix.Signals

sigINFO   :: CInt
sigINFO   = -1

sigWINCH   :: CInt
sigWINCH   = 28


infoEvent :: Signal
infoEvent = sigINFO

windowChange :: Signal
windowChange = sigWINCH
