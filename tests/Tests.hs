{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice
import Fragnix.Nest (writeSlice)

import Test.Tasty (testGroup)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Golden.Manage (defaultMain)

main :: IO ()
main = do
    defaultMain $ testGroup "Tests" [
        goldenVsFile
            "writeSlice putHelloSlice"
            "tests/writeSliceputHelloSlice.golden"
            "fragnix/slices/0"
            (writeSlice putHelloSlice)]

putHelloSlice :: Slice
putHelloSlice = Slice 0 (Binding "main :: IO ()" "main = putHello \"Fragnix!\"")  [putHelloUsage,ioUsage] where
    putHelloUsage = Usage Nothing (VarId "putHello") (OtherSlice 1)
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")

mainSlice :: Slice
mainSlice = Slice 1 (Binding "putHello :: String -> IO ()" "putHello x = putStrLn (\"Hello \" ++ x)") usages where
    usages = [putStrLnUsage,appendUsage,stringUsage,ioUsage]
    putStrLnUsage = Usage Nothing (VarId "putStrLn") (Primitive "System.IO")
    appendUsage = Usage Nothing (VarSym "++") (Primitive "Data.List")
    stringUsage = Usage Nothing (ConId "String") (Primitive "Data.String")
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")