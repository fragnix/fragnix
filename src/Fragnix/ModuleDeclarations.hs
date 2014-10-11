module Fragnix.ModuleDeclarations where

import Fragnix.Declaration (Declaration)

modulDeclarations :: [FilePath] -> IO [Declaration]
modulDeclarations = undefined


{-
extractDeclarations :: ModuleName (Scoped SrcSpan) -> Module (Scoped SrcSpan) -> [Declaration]
extractDeclarations modulenameast annotatedmoduleast =
  map (declToDeclaration modulenameast) (getModuleDecls annotatedmoduleast)

declToDeclaration :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Declaration
declToDeclaration modulenameast annotatedmoduleast = Declaration
    (declGenre annotatedmoduleast)
    (prettyPrint annotatedmoduleast)
    (declaredSymbols modulenameast annotatedmoduleast)
    (usedSymbols annotatedmoduleast)

declGenre :: Decl (Scoped SrcSpan) -> Genre
declGenre (TypeDecl _ _ _) = Type
declGenre (TypeFamDecl _ _ _) = Type
declGenre (DataDecl _ _ _ _ _ _) = Type
declGenre (GDataDecl _ _ _ _ _ _ _) = Type
declGenre (DataFamDecl _ _ _ _) = Type
declGenre (TypeInsDecl _ _ _) = Type
declGenre (DataInsDecl _ _ _ _ _) = Type
declGenre (GDataInsDecl _ _ _ _ _ _) = Type
declGenre (ClassDecl _ _ _ _ _) = TypeClass
declGenre (InstDecl _ _ _ _) = ClassInstance
declGenre (DerivDecl _ _ _) = ClassInstance
declGenre (TypeSig _ _ _) = TypeSignature
declGenre (FunBind _ _) = Value
declGenre (PatBind _ _ _ _ _) = Value
declGenre (ForImp _ _ _ _ _ _) = Value
declGenre _ = Other

declaredSymbols :: ModuleName (Scoped SrcSpan) -> Decl (Scoped SrcSpan) -> Symbols
declaredSymbols modulenameast annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (getTopDeclSymbols GlobalTable.empty modulenameast annotatedmoduleast)

usedSymbols :: Decl (Scoped SrcSpan) -> Symbols
usedSymbols annotatedmoduleast = Symbols (Set.fromList valuesymbols) (Set.fromList typesymbols) where
    (valuesymbols,typesymbols) = partitionEithers (foldMap externalSymbol annotatedmoduleast)

externalSymbol :: Scoped SrcSpan -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
externalSymbol (Scoped (GlobalValue symvalueinfo) _) = [Left symvalueinfo]
externalSymbol (Scoped (GlobalType symtypeinfo) _) = [Right symtypeinfo]
externalSymbol _ = []

data Declaration = Declaration Genre DeclarationAST DeclaredSymbols UsedSymbols deriving (Show,Eq)
data Genre = Value | TypeSignature | Type | TypeClass | ClassInstance | Other deriving (Show,Eq)
type DeclarationAST = String
type DeclaredSymbols = Symbols
type UsedSymbols = Symbols

instance ToJSON Declaration where
    toJSON (Declaration genre declarationast declaredsymbols usedsymbols) = object [
        "declarationgenre" .= show genre,
        "declarationast" .= declarationast,
        "declaredsymbols" .= declaredsymbols,
        "mentionedsymbols" .= usedsymbols]

type ModuleNameString = String

modulePath :: ModuleNameString -> FilePath
modulePath modulename = "/home/pschuster/Projects/fragnix/fragnix" </> "names" </> modulename

builtinPath :: ModuleNameString -> FilePath
builtinPath modulename = "/home/pschuster/Projects/fragnix/fragnix" </> "builtin" </> modulename

newtype FragnixModule a = FragnixModule {runFragnixModule :: IO a}
    deriving (Functor,Monad)

instance MonadModule FragnixModule where
    type ModuleInfo FragnixModule = Symbols
    lookupInCache modulename = FragnixModule (do
        let builtinpath = builtinPath (toFilePath (fromString (modToString modulename)) <.> "names")
            modulepath = modulePath (modToString modulename)
        builtinExists <- doesFileExist builtinpath
        moduleExists <- doesFileExist modulepath
        if builtinExists
            then readInterface builtinpath >>= return . Just
            else (do
                if moduleExists
                    then readInterface modulepath >>= return . Just
                    else return Nothing))
    insertInCache modulename symbols = FragnixModule (do
        let modulepath = modulePath (modToString modulename)
        createDirectoryIfMissing True (dropFileName modulepath)
        writeInterface modulepath symbols)
    getPackages = return []
    readModuleInfo = error "Not implemented: readModuleInfo"


-}
