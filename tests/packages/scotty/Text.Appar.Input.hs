{-# LINE 1 "Text/Appar/Input.hs" #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Appar.Input where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

----------------------------------------------------------------

{-|
  The class for parser input.
-}
class Eq inp => Input inp where
    -- | The head function for input
    car :: inp -> Char
    -- | The tail function for input
    cdr :: inp -> inp
    -- | The end of input
    nil :: inp
    -- | The function to check the end of input
    isNil :: inp -> Bool


instance Input S.ByteString where
    car   = S.head
    cdr   = S.tail
    nil   = S.empty
    isNil = S.null

instance Input L.ByteString where
    car   = L.head
    cdr   = L.tail
    nil   = L.empty
    isNil = L.null

instance Input String where
    car = head
    cdr = tail
    isNil = null
    nil = ""
