module Fragnix.Compiler where

import Fragnix.Slice
import Fragnix.Nest (readSlice)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Name(Ident,Symbol),ImportDecl(ImportDecl),ImportSpec(IVar,IAbs))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (parseDecl,fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Text.IO (writeFile)
import Data.Text (pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Directory (createDirectoryIfMissing)
import System.Process (rawSystem)
import System.Exit (ExitCode)

import Control.Monad (forM_)

assemble :: Slice -> Module
assemble (Slice sliceID slice usages) =
    let decl = fromParseResult . parseDecl . unpack
        decls = case slice of
            Binding signature body -> [decl signature,decl body]
        modulName = ModuleName (sliceModuleName sliceID)
        pragmas = [LanguagePragma noLoc [Ident "NoImplicitPrelude"]]
        imports = map usageImport usages
    in Module noLoc modulName pragmas Nothing Nothing imports decls

usageImport :: Usage -> ImportDecl
usageImport (Usage maybeQualification usedName symbolSource) =
    let modulName = case symbolSource of
            OtherSlice sliceID -> ModuleName (sliceModuleName sliceID)
            Primitive originalModule -> ModuleName (unpack originalModule)
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case usedName of
            VarId name -> IVar (Ident (unpack name))
            VarSym name -> IVar (Symbol (unpack name))
            ConId name -> IAbs (Ident (unpack name))
            ConSym name -> IAbs (Symbol (unpack name))

    in ImportDecl noLoc modulName qualified False Nothing maybeAlias (Just (False,[importSpec]))

sliceModuleDirectory :: FilePath
sliceModuleDirectory = "fragnix" </> "modules"

sliceModulePath :: SliceID -> FilePath
sliceModulePath sliceID = sliceModuleDirectory </> sliceModuleFileName sliceID

sliceModuleFileName :: SliceID -> FilePath
sliceModuleFileName sliceID = sliceModuleName sliceID <.> "hs"

sliceModuleName :: SliceID -> String
sliceModuleName sliceID = "F" ++ show sliceID

writeSliceModule :: Slice -> IO ()
writeSliceModule slice@(Slice sliceID _ _) = writeFile (sliceModulePath sliceID) (pack (prettyPrint (assemble slice)))

assembleTransitive :: SliceID -> IO ()
assembleTransitive sliceID = do
    slice <- readSlice sliceID
    writeSliceModule slice
    forM_ (usedSlices slice) assembleTransitive

compile :: SliceID -> IO ExitCode
compile sliceID = do
    createDirectoryIfMissing True sliceModuleDirectory
    assembleTransitive sliceID
    rawSystem "ghc" ["-o","main","-ifragnix/modules","-main-is",sliceModuleName 0,sliceModulePath 0]

usedSlices :: Slice -> [SliceID]
usedSlices (Slice _ _ usages) = [sliceID | Usage _ _ (OtherSlice sliceID) <- usages]

