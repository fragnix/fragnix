{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Name(Ident),ImportDecl(ImportDecl),ImportSpec(IVar))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (parseDecl,fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Process (rawSystem)

import Control.Monad (forM_)

data Fragment = Fragment FragmentID SourceCode [Usage]

data Usage = Usage (Maybe Qualification) Symbol

data Symbol = Symbol SymbolName FragmentID
            | Primitive SymbolName OriginalModule

type FragmentID = Integer
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text
type SymbolName = Text

get :: FragmentID -> IO Fragment
get 0 = return (Fragment 0 "main = putHello \"Fragnix!\""  [putHelloUsage]) where
    putHelloUsage = Usage Nothing (Symbol "putHello" 1)
get 1 = return (Fragment 1 "putHello x = putStrLn (\"Hello \" ++ x)" [putStrLnUsage,appendUsage]) where
    putStrLnUsage = Usage Nothing (Primitive "putStrLn" "System.IO")
    appendUsage = Usage Nothing (Primitive "(++)" "Data.List")

assemble :: Fragment -> Module
assemble (Fragment fragmentID sourceCode usages) =
    let decl = fromParseResult (parseDecl (unpack sourceCode))
        modulName = ModuleName (fragmentModuleName fragmentID)
        pragmas = [LanguagePragma noLoc [Ident "NoImplicitPrelude"]]
        imports = map usageImport usages
    in Module noLoc modulName pragmas Nothing Nothing imports [decl]

usageImport :: Usage -> ImportDecl
usageImport (Usage maybeQualification symbol) =
    let modulName = case symbol of
            Primitive _ originalModule -> ModuleName (unpack originalModule)
            Symbol _ fragmentID -> ModuleName (fragmentModuleName fragmentID)
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case symbol of
            Primitive symbolName _ -> IVar (Ident (unpack symbolName))
            Symbol symbolName _ -> IVar (Ident (unpack symbolName))
    in ImportDecl noLoc modulName qualified False Nothing maybeAlias (Just (False,[importSpec]))

fragmentPath :: FragmentID -> FilePath
fragmentPath fragmentID = "fragnix" </> fragmentFileName fragmentID

fragmentFileName :: FragmentID -> FilePath
fragmentFileName fragmentID = fragmentModuleName fragmentID <.> "hs"

fragmentModuleName :: FragmentID -> String
fragmentModuleName fragmentID = "F" ++ show fragmentID

compile :: FragmentID -> IO ()
compile fragmentID = do
    fragment <- get fragmentID
    forM_ (usedFragments fragment) compile
    writeFile (fragmentPath fragmentID) (pack (prettyPrint (assemble fragment)))

usedFragments :: Fragment -> [FragmentID]
usedFragments (Fragment _ _ usages) = [fragmentID | Usage _ (Symbol _ fragmentID) <- usages]

main :: IO ()
main = do
    compile 0
    rawSystem "ghc" ["-ifragnix","-main-is",fragmentModuleName 0,fragmentPath 0] >>= print


