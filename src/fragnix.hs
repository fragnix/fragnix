module Main where

import Fragnix.Declaration (writeDeclarations)
import Fragnix.Slice (writeSlice,Slice(Slice))
import Fragnix.Symbol (mainsymbol)
import Fragnix.ModuleDeclarations (modulDeclarations)
import Fragnix.DeclarationSlices (declarationSlices)
import Fragnix.SliceCompiler (sliceCompilerMain,sliceCompiler)

import qualified Data.Map as Map (lookup,keys)
import Control.Monad (forM_,forM)
import System.Environment (getArgs)

fragnixExecutable :: IO ()
fragnixExecutable = do
    args <- getArgs
    declarationmap <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" (Map.keys declarationmap)
    let (slices,globalscope) = declarationSlices declarationmap
    forM_ slices writeSlice
    case Map.lookup mainsymbol globalscope of
        Nothing -> putStrLn "No main slice!"
        Just mainSliceID -> sliceCompilerMain mainSliceID >>= print

fragnixTest :: IO ()
fragnixTest = do
    args <- getArgs
    declarationmap <- modulDeclarations args
    writeDeclarations "fragnix/declarations/declarations.json" (Map.keys declarationmap)
    let (slices,_) = declarationSlices declarationmap
    forM_ slices writeSlice
    let sliceIDs = [sliceID | Slice sliceID _ _ _ <- slices]
    exitCodes <- forM sliceIDs (\sliceID -> sliceCompiler sliceID)
    print (zip sliceIDs exitCodes)

main :: IO ()
main = fragnixTest
