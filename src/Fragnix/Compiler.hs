module Fragnix.Compiler where

import Fragnix.Slice
import Fragnix.Nest (readSlice)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),Decl,ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Name(Ident,Symbol),ImportDecl(ImportDecl),ImportSpec(IVar,IAbs,IThingWith),
    CName(ConName))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (
    parseModuleWithMode,ParseMode(parseFilename),defaultParseMode,fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Directory (createDirectoryIfMissing,doesFileExist)
import System.Process (rawSystem)
import System.Exit (ExitCode)

import Control.Monad (forM_,unless)

compile :: SliceID -> IO ExitCode
compile sliceID = do
    createDirectoryIfMissing True sliceModuleDirectory
    writeSliceModuleTransitive sliceID
    rawSystem "ghc" ["-o","main","-ifragnix/modules","-main-is",sliceModuleName sliceID,sliceModulePath sliceID]

assemble :: Slice -> Module
assemble (Slice sliceID slice usages) =
    let decls = case slice of
            Fragment declarations -> map (parseDeclaration sliceID) declarations
        modulName = ModuleName (sliceModuleName sliceID)
        pragmas = [LanguagePragma noLoc [Ident "NoImplicitPrelude"]]
        imports = map usageImport usages
    in Module noLoc modulName pragmas Nothing Nothing imports decls

parseDeclaration :: SliceID -> Text -> Decl
parseDeclaration sliceID declaration = decl where
    Module _ _ _ _ _ _ [decl] = fromParseResult (parseModuleWithMode parseMode (unpack declaration))
    parseMode = defaultParseMode { parseFilename = show sliceID }

usageImport :: Usage -> ImportDecl
usageImport (Usage maybeQualification usedName symbolSource) =
    let modulName = case symbolSource of
            OtherSlice sliceID -> ModuleName (sliceModuleName sliceID)
            Primitive originalModule -> ModuleName (unpack originalModule)
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case usedName of
            ValueIdentifier name -> IVar (Ident (unpack name))
            ValueOperator name -> IVar (Symbol (unpack name))
            TypeIdentifier name -> IAbs (Ident (unpack name))
            TypeOperator name -> IAbs (Symbol (unpack name))
            ConstructorIdentifier typeName name ->
                IThingWith (Ident (unpack typeName)) [(ConName (Ident (unpack name)))]
            ConstructorOperator typeName name ->
                IThingWith (Ident (unpack typeName)) [(ConName (Symbol (unpack name)))]

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

writeSliceModuleTransitive :: SliceID -> IO ()
writeSliceModuleTransitive sliceID = do
    exists <- doesSliceModuleExist sliceID
    unless exists (do
        slice <- readSlice sliceID
        writeSliceModule slice
        forM_ (usedSlices slice) writeSliceModuleTransitive)

usedSlices :: Slice -> [SliceID]
usedSlices (Slice _ _ usages) = [sliceID | Usage _ _ (OtherSlice sliceID) <- usages]

doesSliceModuleExist :: SliceID -> IO Bool
doesSliceModuleExist sliceID = doesFileExist (sliceModulePath sliceID)
