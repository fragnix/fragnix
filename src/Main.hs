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

import Control.Category ((>>>))

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
get 0 = return (Fragment 0 "main = putStrLn \"Hello Fragnix!\"" [putStrLnUsage]) where
    putStrLnUsage = Usage Nothing (Primitive "putStrLn" "System.IO")

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
compile fragmentID = get fragmentID >>= (assemble >>> prettyPrint >>> pack >>> return) >>= writeFile (fragmentPath fragmentID)

main :: IO ()
main = compile 0


