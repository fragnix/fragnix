{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Name(Ident,Symbol),ImportDecl(ImportDecl),ImportSpec(IVar))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (parseDecl,fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Process (rawSystem)

import Control.Monad (forM_)

data Fragment = Fragment FragmentID SourceCode [Usage]

data Usage = Usage (Maybe Qualification) Name SymbolSource

data SymbolSource = OtherFragment FragmentID
                  | Primitive OriginalModule

type FragmentID = Integer
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text

get :: FragmentID -> IO Fragment
get 0 = return (Fragment 0 "main = putHello \"Fragnix!\""  [putHelloUsage]) where
    putHelloUsage = Usage Nothing (Ident "putHello") (OtherFragment 1)
get 1 = return (Fragment 1 "putHello x = putStrLn (\"Hello \" ++ x)" [putStrLnUsage,appendUsage]) where
    putStrLnUsage = Usage Nothing (Ident "putStrLn") (Primitive "System.IO")
    appendUsage = Usage Nothing (Symbol "++") (Primitive "Data.List")

assemble :: Fragment -> Module
assemble (Fragment fragmentID sourceCode usages) =
    let decl = fromParseResult (parseDecl (unpack sourceCode))
        modulName = ModuleName (fragmentModuleName fragmentID)
        pragmas = [LanguagePragma noLoc [Ident "NoImplicitPrelude"]]
        imports = map usageImport usages
    in Module noLoc modulName pragmas Nothing Nothing imports [decl]

usageImport :: Usage -> ImportDecl
usageImport (Usage maybeQualification name symbolSource) =
    let modulName = case symbolSource of
            OtherFragment fragmentID -> ModuleName (fragmentModuleName fragmentID)
            Primitive originalModule -> ModuleName (unpack originalModule)
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = IVar name
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
usedFragments (Fragment _ _ usages) = [fragmentID | Usage _ _ (OtherFragment fragmentID) <- usages]

main :: IO ()
main = do
    compile 0
    rawSystem "ghc" ["-ifragnix","-main-is",fragmentModuleName 0,fragmentPath 0] >>= print


