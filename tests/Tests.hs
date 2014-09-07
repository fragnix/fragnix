{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice
import Fragnix.Nest (writeSlice,readSlice)
import Fragnix.Compiler (writeSliceModule,compile)

import Test.Tasty (testGroup,TestTree)
import Test.Tasty.Golden (goldenVsFile)
import Test.Tasty.Golden.Manage (defaultMain)

main :: IO ()
main = do
    defaultMain (testGroup "Tests" [
        writeSliceTests,
        readSliceTests,
        writeSliceModuleTests,
        compileTest])

writeSliceTests :: TestTree
writeSliceTests = testGroup "writeSlice" [
    goldenVsFile
        "putHelloSlice"
        "tests/examples/writeSliceputHelloSlice.golden"
        "fragnix/slices/0"
        (writeSlice putHelloSlice),
    goldenVsFile
        "mainSlice"
        "tests/examples/writeSlicemainSlice.golden"
        "fragnix/slices/1"
        (writeSlice mainSlice)]

readSliceTests :: TestTree
readSliceTests = testGroup "readSlice" [
    goldenVsFile
        "putHelloSlice"
        "tests/examples/readSliceputHelloSlice.golden"
        "tests/examples/readSliceputHelloSlice.out"
        (readSlice 0 >>= writeFile "tests/examples/readSliceputHelloSlice.out" . show),
    goldenVsFile
        "mainSlice"
        "tests/examples/readSlicemainSlice.golden"
        "tests/examples/readSlicemainSlice.out"
        (readSlice 1 >>= writeFile "tests/examples/readSlicemainSlice.out" . show)]

writeSliceModuleTests :: TestTree
writeSliceModuleTests = testGroup "writeSliceModule" [
    goldenVsFile
        "putHelloSlice"
        "tests/examples/writeSliceModuleputHelloSlice.golden"
        "fragnix/modules/F0.hs"
        (writeSliceModule putHelloSlice),
    goldenVsFile
        "mainSlice"
        "tests/examples/writeSliceModulemainSlice.golden"
        "fragnix/modules/F1.hs"
        (writeSliceModule mainSlice)]

compileTest :: TestTree
compileTest = testGroup "compile" [
    goldenVsFile
        "compile"
        "tests/examples/compile.golden"
        "tests/examples/compile.out"
        (compile 0 >>= writeFile "tests/examples/compile.out" . show)]

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
