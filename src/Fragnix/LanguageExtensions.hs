module Fragnix.LanguageExtensions where

import Language.Haskell.Exts.Extension (
    Extension(EnableExtension),glasgowExts,KnownExtension(NondecreasingIndentation))

languageExtensions :: [Extension]
languageExtensions = EnableExtension NondecreasingIndentation:glasgowExts
