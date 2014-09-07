{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Nest where

import Fragnix.Slice

data NestError = SliceNotFound SliceID

get :: SliceID -> IO Slice
get 0 = return (Slice 0 (Binding "main :: IO ()" "main = putHello \"Fragnix!\"")  [putHelloUsage,ioUsage]) where
    putHelloUsage = Usage Nothing (VarId "putHello") (OtherSlice 1)
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")
get 1 = return (Slice 1 (Binding "putHello :: String -> IO ()" "putHello x = putStrLn (\"Hello \" ++ x)") usages) where
    usages = [putStrLnUsage,appendUsage,stringUsage,ioUsage]
    putStrLnUsage = Usage Nothing (VarId "putStrLn") (Primitive "System.IO")
    appendUsage = Usage Nothing (VarSym "++") (Primitive "Data.List")
    stringUsage = Usage Nothing (ConId "String") (Primitive "Data.String")
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")
