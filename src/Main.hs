{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Name(Ident,Symbol),ImportDecl(ImportDecl),ImportSpec(IVar,IAbs))
import Language.Haskell.Exts.SrcLoc (noLoc)
import Language.Haskell.Exts.Parser (parseDecl,fromParseResult)
import Language.Haskell.Exts.Pretty (prettyPrint)

import Data.Text.IO (writeFile)
import Data.Text (Text,pack,unpack)

import System.FilePath ((</>),(<.>))
import System.Process (rawSystem)

import Control.Monad (forM_)

data Fragment = Fragment FragmentID FragmentDeclaration [Usage]

data FragmentDeclaration = Binding Signature SourceCode

data Usage = Usage (Maybe Qualification) UsedName Reference

data UsedName = Variable Name
              | Abstract Name

data Reference = OtherFragment FragmentID
               | Primitive OriginalModule

type FragmentID = Integer
type SourceCode = Text
type Qualification = Text
type OriginalModule = Text
type Signature = SourceCode

get :: FragmentID -> IO Fragment
get 0 = return (Fragment 0 (Binding "main :: IO ()" "main = putHello \"Fragnix!\"")  [putHelloUsage,ioUsage]) where
    putHelloUsage = Usage Nothing (Variable (Ident "putHello")) (OtherFragment 1)
    ioUsage = Usage Nothing (Abstract (Ident "IO")) (Primitive "System.IO")
get 1 = return (Fragment 1 (Binding "putHello :: String -> IO ()" "putHello x = putStrLn (\"Hello \" ++ x)") usages) where
    usages = [putStrLnUsage,appendUsage,stringUsage,ioUsage]
    putStrLnUsage = Usage Nothing (Variable (Ident "putStrLn")) (Primitive "System.IO")
    appendUsage = Usage Nothing (Variable (Symbol "++")) (Primitive "Data.List")
    stringUsage = Usage Nothing (Abstract (Ident "String")) (Primitive "Data.String")
    ioUsage = Usage Nothing (Abstract (Ident "IO")) (Primitive "System.IO")

assemble :: Fragment -> Module
assemble (Fragment fragmentID fragmentDeclaration usages) =
    let decl = fromParseResult . parseDecl . unpack
        decls = case fragmentDeclaration of
            Binding signature body -> [decl signature,decl body]
        modulName = ModuleName (fragmentModuleName fragmentID)
        pragmas = [LanguagePragma noLoc [Ident "NoImplicitPrelude"]]
        imports = map usageImport usages
    in Module noLoc modulName pragmas Nothing Nothing imports decls

usageImport :: Usage -> ImportDecl
usageImport (Usage maybeQualification usedName symbolSource) =
    let modulName = case symbolSource of
            OtherFragment fragmentID -> ModuleName (fragmentModuleName fragmentID)
            Primitive originalModule -> ModuleName (unpack originalModule)
        qualified = maybe False (const True) maybeQualification
        maybeAlias = fmap (ModuleName . unpack) maybeQualification
        importSpec = case usedName of
            Variable name -> IVar name
            Abstract name -> IAbs name
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


