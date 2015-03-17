{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Network/Sendfile/Types.hs" #-}
module Network.Sendfile.Types where

-- |
--  File range for 'sendfile'.

data FileRange = EntireFile
               | PartOfFile {
                   rangeOffset :: Integer
                 , rangeLength :: Integer
                 }
