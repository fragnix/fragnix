{-# LANGUAGE OverloadedStrings #-}
module Fragnix.Nest where

import Fragnix.Slice

import Language.Haskell.Exts.Syntax (Name(Ident,Symbol))

get :: SliceID -> IO Slice
get 0 = return (Slice 0 (Binding "main :: IO ()" "main = putHello \"Fragnix!\"")  [putHelloUsage,ioUsage]) where
    putHelloUsage = Usage Nothing (Variable (Ident "putHello")) (OtherSlice 1)
    ioUsage = Usage Nothing (Abstract (Ident "IO")) (Primitive "System.IO")
get 1 = return (Slice 1 (Binding "putHello :: String -> IO ()" "putHello x = putStrLn (\"Hello \" ++ x)") usages) where
    usages = [putStrLnUsage,appendUsage,stringUsage,ioUsage]
    putStrLnUsage = Usage Nothing (Variable (Ident "putStrLn")) (Primitive "System.IO")
    appendUsage = Usage Nothing (Variable (Symbol "++")) (Primitive "Data.List")
    stringUsage = Usage Nothing (Abstract (Ident "String")) (Primitive "Data.String")
    ioUsage = Usage Nothing (Abstract (Ident "IO")) (Primitive "System.IO")
