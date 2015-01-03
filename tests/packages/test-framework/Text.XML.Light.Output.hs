{-# LINE 1 "./Text/XML/Light/Output.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Text/XML/Light/Output.hs" #-}
{-# LINE 1 "./Text/XML/Light/Output.hs" #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Output
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- Output handling for the lightweight XML lib.
--

module Text.XML.Light.Output
  ( showTopElement, showContent, showElement, showCData, showQName, showAttr
  , ppTopElement, ppContent, ppElement
  , ppcTopElement, ppcContent, ppcElement
  , ConfigPP
  , defaultConfigPP, prettyConfigPP
  , useShortEmptyTags, useExtraWhiteSpace
  , tagEnd, xml_header
  ) where

import Text.XML.Light.Types
import Data.Char
import Data.List ( isPrefixOf )

-- | The XML 1.0 header
xml_header :: String
xml_header = "<?xml version='1.0' ?>"


--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { shortEmptyTag :: QName -> Bool
  , prettify      :: Bool
  }

-- | Default pretty orinting configutaion.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { shortEmptyTag = const True
                           , prettify      = False
                           }

-- | The predicate specifies for which empty tags we should use XML's
-- abbreviated notation <TAG />.  This is useful if we are working with
-- some XML-ish standards (such as certain versions of HTML) where some
-- empty tags should always be displayed in the <TAG></TAG> form.
useShortEmptyTags :: (QName -> Bool) -> ConfigPP -> ConfigPP
useShortEmptyTags p c = c { shortEmptyTag = p }


-- | Specify if we should use extra white-space to make document more readable.
-- WARNING: This adds additional white-space to text elements,
-- and so it may change the meaning of the document.
useExtraWhiteSpace :: Bool -> ConfigPP -> ConfigPP
useExtraWhiteSpace p c  = c { prettify = p }

-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = useExtraWhiteSpace True defaultConfigPP


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> String
ppTopElement        = ppcTopElement prettyConfigPP

-- | Pretty printing elements
ppElement          :: Element -> String
ppElement           = ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> String
ppContent           = ppcContent prettyConfigPP



-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> String
ppcTopElement c e   = unlines [xml_header,ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> String
ppcElement c e      = ppElementS c "" e ""

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> String
ppcContent c x      = ppContentS c "" x ""





-- | Pretty printing content using ShowS
ppContentS         :: ConfigPP -> String -> Content -> ShowS
ppContentS c i x xs = case x of
                        Elem e -> ppElementS c i e xs
                        Text t -> ppCDataS c i t xs
                        CRef r -> showCRefS r xs

ppElementS         :: ConfigPP -> String -> Element -> ShowS
ppElementS c i e xs = i ++ (tagStart (elName e) (elAttribs e) $
  case elContent e of
    [] | "?" `isPrefixOf` qName name -> " ?>" ++ xs
       | shortEmptyTag c name  -> " />" ++ xs
    [Text t] -> ">" ++ ppCDataS c "" t (tagEnd name xs)
    cs -> '>' : nl ++ foldr ppSub (i ++ tagEnd name xs) cs
      where ppSub e1 = ppContentS c (sp ++ i) e1 . showString nl
            (nl,sp)  = if prettify c then ("\n","  ") else ("","")
  )
  where name = elName e

ppCDataS           :: ConfigPP -> String -> CData -> ShowS
ppCDataS c i t xs   = i ++ if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t xs
                             else foldr cons xs (showCData t)

  where cons         :: Char -> String -> String
        cons '\n' ys  = "\n" ++ i ++ ys
        cons y ys     = y : ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> String
showTopElement c    = xml_header ++ showElement c

showContent        :: Content -> String
showContent c       = ppContentS defaultConfigPP "" c ""

showElement        :: Element -> String
showElement c       = ppElementS defaultConfigPP "" c ""

showCData          :: CData -> String
showCData c         = ppCDataS defaultConfigPP "" c ""

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: String -> ShowS
showCRefS r xs      = '&' : r ++ ';' : xs

-- | Convert a text element to characters.
showCDataS         :: CData -> ShowS
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> showString "<![CDATA[" . escCData (cdData cd)
                                           . showString "]]>"
   CDataRaw      -> \ xs -> cdData cd ++ xs

--------------------------------------------------------------------------------
escCData           :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

escChar            :: Char -> ShowS
escChar c = case c of
  '<'   -> showString "&lt;"
  '>'   -> showString "&gt;"
  '&'   -> showString "&amp;"
  '"'   -> showString "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> showString "&#39;"

  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> showChar c
    | otherwise -> showString "&#" . shows oc . showChar ';'
      where oc = ord c

escStr             :: String -> ShowS
escStr cs rs        = foldr escChar rs cs

tagEnd             :: QName -> ShowS
tagEnd qn rs        = '<':'/':showQName qn ++ '>':rs

tagStart           :: QName -> [Attr] -> ShowS
tagStart qn as rs   = '<':showQName qn ++ as_str ++ rs
 where as_str       = if null as then "" else ' ' : unwords (map showAttr as)

showAttr           :: Attr -> String
showAttr (Attr qn v) = showQName qn ++ '=' : '"' : escStr v "\""

showQName          :: QName -> String
showQName q         = pre ++ qName q
  where pre = case qPrefix q of
                Nothing -> ""
                Just p  -> p ++ ":"



