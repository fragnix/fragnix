{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Signals/Exts.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                      






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "dist/dist-sandbox-235ea54e/build/System/Posix/Signals/Exts.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/System/Posix/Signals/Exts.hs" #-}
{-# LINE 1 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 4 "System/Posix/Signals/Exts.hsc" #-}
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


{-# LINE 19 "System/Posix/Signals/Exts.hsc" #-}

module System.Posix.Signals.Exts (
  module System.Posix.Signals


{-# LINE 26 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 27 "System/Posix/Signals/Exts.hsc" #-}
  , windowChange, sigWINCH

{-# LINE 29 "System/Posix/Signals/Exts.hsc" #-}

  ) where

import Foreign.C
import System.Posix.Signals


{-# LINE 43 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 46 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 47 "System/Posix/Signals/Exts.hsc" #-}
foreign import ccall unsafe "__hsunix_SIGWINCH"   sigWINCH   :: CInt

{-# LINE 49 "System/Posix/Signals/Exts.hsc" #-}

{-# LINE 50 "System/Posix/Signals/Exts.hsc" #-}


{-# LINE 55 "System/Posix/Signals/Exts.hsc" #-}


{-# LINE 57 "System/Posix/Signals/Exts.hsc" #-}
windowChange :: Signal
windowChange = sigWINCH

{-# LINE 60 "System/Posix/Signals/Exts.hsc" #-}
