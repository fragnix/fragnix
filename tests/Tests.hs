{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice
import Fragnix.Resolver (resolve)
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
        compileTest,
        pipelineTests])

pipelineTests :: TestTree
pipelineTests = testGroup "pipelineTests" (map pipelineTest [
    "HelloFragnix.hs",
    "HiFragnix.hs"])

pipelineTest :: FilePath -> TestTree
pipelineTest fileName = testGroup fileName [
    goldenVsFile
        fileName
        (filePath ++ ".golden")
        (filePath ++ ".out")
        (fragnix filePath)] where
            filePath = "tests/examples/"++fileName

fragnix :: FilePath -> IO ()
fragnix filePath = do
    (slices,mainID) <- resolve filePath
    mapM writeSlice slices
    exitCode <- compile mainID
    writeFile (filePath ++ ".out") (show exitCode)

writeSliceTests :: TestTree
writeSliceTests = testGroup "writeSlice" [
    goldenVsFile
        "mainSlice"
        "tests/examples/writeSlicemainSlice.golden"
        "fragnix/slices/0"
        (writeSlice mainSlice),
    goldenVsFile
        "putHelloSlice"
        "tests/examples/writeSliceputHelloSlice.golden"
        "fragnix/slices/1"
        (writeSlice putHelloSlice)]

readSliceTests :: TestTree
readSliceTests = testGroup "readSlice" [
    goldenVsFile
        "mainSlice"
        "tests/examples/readSlicemainSlice.golden"
        "tests/examples/readSlicemainSlice.out"
        (readSlice 0 >>= writeFile "tests/examples/readSlicemainSlice.out" . show),
    goldenVsFile
        "putHelloSlice"
        "tests/examples/readSliceputHelloSlice.golden"
        "tests/examples/readSliceputHelloSlice.out"
        (readSlice 1 >>= writeFile "tests/examples/readSliceputHelloSlice.out" . show)]

writeSliceModuleTests :: TestTree
writeSliceModuleTests = testGroup "writeSliceModule" [
    goldenVsFile
        "mainSlice"
        "tests/examples/writeSliceModulemainSlice.golden"
        "fragnix/modules/F0.hs"
        (writeSliceModule mainSlice),
    goldenVsFile
        "putHelloSlice"
        "tests/examples/writeSliceModuleputHelloSlice.golden"
        "fragnix/modules/F1.hs"
        (writeSliceModule putHelloSlice)]

compileTest :: TestTree
compileTest = testGroup "compile" [
    goldenVsFile
        "compile"
        "tests/examples/compile.golden"
        "tests/examples/compile.out"
        (compile 0 >>= writeFile "tests/examples/compile.out" . show)]

mainSlice :: Slice
mainSlice = Slice 0 (Fragment ["main :: IO ()","main = putHello \"Fragnix!\""])  [putHelloUsage,ioUsage] where
    putHelloUsage = Usage Nothing (VarId "putHello") (OtherSlice 1)
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")

putHelloSlice :: Slice
putHelloSlice = Slice 1 (Fragment ["putHello :: String -> IO ()","putHello x = putStrLn (\"Hello \" ++ x)"]) usages where
    usages = [putStrLnUsage,appendUsage,stringUsage,ioUsage]
    putStrLnUsage = Usage Nothing (VarId "putStrLn") (Primitive "System.IO")
    appendUsage = Usage Nothing (VarSym "++") (Primitive "Data.List")
    stringUsage = Usage Nothing (ConId "String") (Primitive "Data.String")
    ioUsage = Usage Nothing (ConId "IO") (Primitive "System.IO")
