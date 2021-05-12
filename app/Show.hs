{-# LANGUAGE NamedFieldPuns #-}

module Show (showInformation) where

import Fragnix.Core.Loaf (Loaf (..), Name (..), Symbol (..))
import Fragnix.Core.Slice
    (Fragment (..), Language (..), Reference (..), Slice (..), Use (..),
    UsedName (..))
import qualified Fragnix.Core.Slice as F (Name (..))

import Data.List (intercalate)
import Data.Text (unpack)

import Network.HTTP.Req
import Utils (IDType (..), loafRequest, sliceRequest)

showInformation :: IDType -> IO ()
showInformation (SliceID sliceID) = do
  r <- sliceRequest sliceID
  putStrLn $ prettyPrintSlice (responseBody r :: Slice)
showInformation (LoafID loafID) = do
  r <- loafRequest loafID
  putStrLn $ prettyPrintLoaf (responseBody r :: Loaf)


prettyPrintSlice :: Slice -> String
prettyPrintSlice sl = "Slice " ++ unpack (sliceID sl) ++ "\n"
                    ++ "--------------------------\n\n"
                    ++ fragmentToString (fragment sl) ++ "\n\n"
                    ++ "--------------------------\n"
                    ++ "Language Extensions:\n"
                    ++ languageToString (language sl) ++ "\n\n"
                    ++ "Dependencies:\n"
                    ++ usesToString (uses sl)

fragmentToString :: Fragment -> String
fragmentToString (Fragment sc) = intercalate "\n" (map unpack sc)

languageToString :: Language -> String
languageToString (Language exts) = intercalate "\n" (map (\e -> "    " ++ unpack e) exts)

usesToString :: [Use] -> String
usesToString us = intercalate "\n" (map (\e -> "    " ++ useToString e) us)

useToString :: Use -> String
useToString (Use Nothing name ref) = usedNameToString name ++ " from " ++ refToString ref
useToString (Use (Just qual) name ref) = usedNameToString name ++ " as " ++ unpack qual ++ " from " ++ refToString ref

usedNameToString :: UsedName -> String
usedNameToString (ConstructorName _ n) = nameToString n
usedNameToString (ValueName n)         = nameToString n
usedNameToString (TypeName n)          = nameToString n

nameToString :: F.Name -> String
nameToString (F.Identifier n) = unpack n
nameToString (F.Operator n)   = "(" ++ unpack n ++ ")"

refToString :: Reference -> String
refToString (OtherSlice s)   = "Slice " ++ unpack s
refToString (Builtin m)      = unpack m ++ " (builtin)"
refToString (ForeignSlice f) = unpack f ++ " (foreign)"


prettyPrintLoaf :: Loaf -> String
prettyPrintLoaf Loaf{name, symbols} = "Loaf " ++ unpack name ++ "\n"
                            ++ "----------------------------\n"
                            ++ intercalate "\n" (map symbolToString symbols)

symbolToString :: Symbol -> String
symbolToString s = symbolModuleToString (symbolModule s) ++ ": " ++ symbolNameToString (symbolName s)

symbolNameToString :: Name -> String
symbolNameToString (Ident s)  = unpack s
symbolNameToString (Symbol s) = unpack s

symbolModuleToString :: Reference -> String
symbolModuleToString (OtherSlice s)   = unpack s
symbolModuleToString (Builtin s)      = unpack s
symbolModuleToString (ForeignSlice s) = unpack s
