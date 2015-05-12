{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, Rank2Types,
  MultiParamTypeClasses, FlexibleContexts, TypeFamilies,
  ScopedTypeVariables, BangPatterns, NondecreasingIndentation,
  ExplicitForAll, PatternGuards #-}
module F7847327753362127975 where
import GHC.Show (showParen)
import GHC.Classes ((>))
import GHC.Base (($))
import GHC.Show (showString)
import GHC.Base ((.))
import GHC.Show (shows)
import {-# SOURCE #-} F262630417951198712 (Vector)
import GHC.Show (Show)
import GHC.Types (Int)
import GHC.Show (ShowS)

showsPrec :: (Vector v a, Show a) => Int -> v a -> ShowS