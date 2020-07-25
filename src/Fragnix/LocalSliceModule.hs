module Fragnix.LocalSliceModule () where

import Fragnix.LocalSlice (
    LocalSlice(LocalSlice),LocalSliceID(LocalSliceID),LocalUse(LocalUse),
    LocalReference(OtherSlice,Builtin,OtherLocalSlice))
import Fragnix.Slice (
    Language(Language),Fragment(Fragment),
    UsedName(ValueName,TypeName,ConstructorName),Name(Identifier,Operator),
    sliceIDModuleName)

import Prelude hiding (writeFile)

import Language.Haskell.Exts.Syntax (
    Module(Module),ModuleHead(ModuleHead),
    ModuleName(ModuleName),ModulePragma(LanguagePragma),
    Decl,Name(Ident,Symbol),
    ImportSpecList(ImportSpecList),ImportDecl(ImportDecl),
    ImportSpec(IVar,IAbs,IThingWith),
    CName(ConName),Namespace(NoNamespace))
import Language.Haskell.Exts.Parser (
    parseModuleWithMode,ParseMode(parseFilename,extensions,fixities),defaultParseMode,
    fromParseResult)
import Language.Haskell.Exts.Fixity (baseFixities)
import Language.Haskell.Exts.Extension (parseExtension)

import Data.Text (Text,unpack)
import Data.Maybe (isJust)


-- | Given a local slice generate the corresponding module.
localSliceModule :: LocalSlice -> Module ()
localSliceModule (LocalSlice localSliceID language fragment localUses _) =
    let Fragment declarations = fragment
        moduleHead = ModuleHead () moduleName Nothing Nothing
        pragmas = [LanguagePragma () languagepragmas]
        imports = map useImport localUses
        decls = map (parseDeclaration localSliceID ghcextensions) declarations
        moduleName = ModuleName () (localSliceModuleName localSliceID)
        languagepragmas = map (Ident () . unpack) ghcextensions
        Language ghcextensions = language
    in Module () (Just moduleHead) pragmas imports decls


-- | Reparse a declaration from its textual representation in a local slice.
parseDeclaration :: LocalSliceID -> [Text] -> Text -> Decl ()
parseDeclaration localSliceID ghcextensions declaration = fmap (const ()) decl where
    -- Workaround because HSE couldn't parse all standalone declarations.
    Module _ _ _ _ [decl] = fromParseResult (parseModuleWithMode parseMode (unpack declaration))
    parseMode = defaultParseMode {
        parseFilename = show localSliceID,
        extensions = map (parseExtension . unpack) ghcextensions,
        fixities = Just baseFixities}


-- | Import decl for a given use of a symbol.
useImport :: LocalUse -> ImportDecl ()
useImport (LocalUse maybeQualification usedName symbolSource) =
    let moduleName = case symbolSource of
            OtherSlice sliceID -> ModuleName () (sliceIDModuleName sliceID)
            Builtin originalModule -> ModuleName () (unpack originalModule)
            OtherLocalSlice localSliceID -> ModuleName () (localSliceModuleName localSliceID)
        qualified = isJust maybeQualification
        maybeAlias = fmap (ModuleName () . unpack) maybeQualification
        importSpec = case usedName of
            ValueName name ->
                IVar () (toName name)
            TypeName name ->
                IAbs () (NoNamespace ()) (toName name)
            ConstructorName typeName name ->
                IThingWith () (toName typeName) [ConName () (toName name)]
        importSpecList = Just (ImportSpecList () False [importSpec])
        toName (Identifier name) = Ident () (unpack name)
        toName (Operator name) = Symbol () (unpack name)

    in ImportDecl () moduleName qualified False False Nothing maybeAlias importSpecList

localSliceModuleName :: LocalSliceID -> String
localSliceModuleName (LocalSliceID localSliceID) = unpack localSliceID


