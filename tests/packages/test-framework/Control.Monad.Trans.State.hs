{-# LINE 1 "./Control/Monad/Trans/State.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Control/Monad/Trans/State.hs" #-}
{-# LINE 1 "./Control/Monad/Trans/State.hs" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.State
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- State monads, passing an updatable state through a computation.
--
-- Some computations may not require the full power of state transformers:
--
-- * For a read-only state, see "Control.Monad.Trans.Reader".
--
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Trans.Writer".
--
-- This version is lazy; for a strict version, see
-- "Control.Monad.Trans.State.Strict", which has the same interface.
-----------------------------------------------------------------------------

module Control.Monad.Trans.State (
  module Control.Monad.Trans.State.Lazy
  ) where

import Control.Monad.Trans.State.Lazy
