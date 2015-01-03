{-# LINE 1 "./Control/Monad/List.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                  






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Control/Monad/List.hs" #-}
{-# LINE 1 "./Control/Monad/List.hs" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.List
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The List monad.
--
-----------------------------------------------------------------------------

module Control.Monad.List (
    ListT(..),
    mapListT,
    module Control.Monad,
    module Control.Monad.Trans,
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.List
