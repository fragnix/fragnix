{-# LANGUAGE StandaloneDeriving #-}
module Fragnix.Resolver where

import Fragnix.Slice

import Language.Haskell.Exts.Annotated (
    parseFile,fromParseResult,Language(Haskell2010),prettyPrint,
    Module(Module),SrcSpanInfo,Decl,ModuleName)
import Language.Haskell.Names (
    annotateModule,Scoped(Scoped),NameInfo(GlobalValue,GlobalType),
    OrigName(OrigName),GName(GName),SymValueInfo(SymValue),SymTypeInfo)
import Language.Haskell.Names.Interfaces (evalNamesModuleT)
import Language.Haskell.Names.SyntaxUtils (getModuleDecls,getModuleName)
import Language.Haskell.Names.ModuleSymbols (getTopDeclSymbols)
import qualified Language.Haskell.Names.GlobalSymbolTable as GlobalTable (empty)

import Data.Text (pack)
import Data.Functor ((<$>))
import Data.Foldable (foldMap)
import Control.Monad (void)


extractSlices :: FilePath -> IO [Slice]
extractSlices filePath = do
    originalModule <- fromParseResult <$> parseFile filePath
    scopedModule <- evalNamesModuleT (annotateModule Haskell2010 [] originalModule) []
    let nameInfoModule = fmap (\(Scoped nameInfo _) -> void nameInfo) scopedModule
    print (map (boundSymbols (getModuleName nameInfoModule)) (getModuleDecls nameInfoModule))
    return []

data BoundSymbol = BoundSymbol Entity OriginalModule UsedName

deriving instance Show BoundSymbol
deriving instance Eq BoundSymbol
deriving instance Ord BoundSymbol

data Entity = ValueEntity | TypeEntity

deriving instance Show Entity
deriving instance Eq Entity
deriving instance Ord Entity

boundSymbols :: ModuleName (NameInfo ()) -> Decl (NameInfo ()) -> [BoundSymbol]
boundSymbols modulName = map infoToBoundSymbol . getTopDeclSymbols GlobalTable.empty modulName

infoToBoundSymbol :: Either (SymValueInfo OrigName) (SymTypeInfo OrigName) -> BoundSymbol
infoToBoundSymbol (Left (SymValue (OrigName _ (GName originalModule boundName)) _)) =
    BoundSymbol ValueEntity (pack originalModule) (VarId (pack boundName))

extractDeclarations :: Module (Scoped SrcSpanInfo) -> [String]
extractDeclarations modul = map (show . fmap (\(Scoped ni _) -> fmap (const ()) ni)) (getModuleDecls modul)

extractMentionedSymbols :: Module (Scoped SrcSpanInfo) -> [[String]]
extractMentionedSymbols modul = map (foldMap externalSymbol) (getModuleDecls modul)
    

 --   show . getTopDeclSymbols GlobalTable.empty (getModuleName modul)) (getModuleDecls modul)

externalSymbol :: Scoped l -> [String]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [show symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [show symtypeinfo]
externalSymbol _ = []
