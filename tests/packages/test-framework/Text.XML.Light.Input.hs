{-# LINE 1 "./Text/XML/Light/Input.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Text/XML/Light/Input.hs" #-}
{-# LINE 1 "./Text/XML/Light/Input.hs" #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Input
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Lightweight XML parsing
--

module Text.XML.Light.Input (parseXML,parseXMLDoc) where

import Text.XML.Light.Lexer
import Text.XML.Light.Types
import Text.XML.Light.Proc
import Text.XML.Light.Output(tagEnd)

import Data.List(isPrefixOf)

-- | parseXMLDoc, parse a XMLl document to maybe an element
parseXMLDoc  :: XmlSource s => s -> Maybe Element
parseXMLDoc xs  = strip (parseXML xs)
  where strip cs = case onlyElems cs of
                    e : es
                      | "?xml" `isPrefixOf` qName (elName e)
                          -> strip (map Elem es)
                      | otherwise -> Just e
                    _ -> Nothing

-- | parseXML to a list of content chunks
parseXML :: XmlSource s => s -> [Content]
parseXML  = parse . tokens

------------------------------------------------------------------------

parse      :: [Token] -> [Content]
parse []    = []
parse ts    = let (es,_,ts1) = nodes ([],Nothing) [] ts
              in es ++ parse ts1

-- Information about namespaces.
-- The first component is a map that associates prefixes to URIs,
-- the second is the URI for the default namespace, if one was provided.
type NSInfo = ([(String,String)],Maybe String)

nodes :: NSInfo -> [QName] -> [Token] -> ([Content], [QName], [Token])

nodes ns ps (TokCRef ref : ts) =
  let (es,qs,ts1) = nodes ns ps ts
  in (CRef ref : es, qs, ts1)

nodes ns ps (TokText txt : ts) =
  let (es,qs,ts1) = nodes ns ps ts
      (more,es1)  = case es of
                      Text cd : es1'
                        | cdVerbatim cd == cdVerbatim txt -> (cdData cd,es1')
                      _                                   -> ([],es)

  in (Text txt { cdData = cdData txt ++ more } : es1, qs, ts1)

nodes cur_info ps (TokStart p t as empty : ts) = (node : siblings, open, toks)
  where
  new_name  = annotName new_info t
  new_info  = foldr addNS cur_info as
  node      = Elem Element { elLine    = Just p
                           , elName    = new_name
                           , elAttribs = map (annotAttr new_info) as
                           , elContent = children
                           }

  (children,(siblings,open,toks))
    | empty     = ([], nodes cur_info ps ts)
    | otherwise = let (es1,qs1,ts1) = nodes new_info (new_name:ps) ts
                  in (es1,
                      case qs1 of
                        [] -> nodes cur_info ps ts1
                        _ : qs3 -> ([],qs3,ts1))

nodes ns ps (TokEnd p t : ts)   = let t1 = annotName ns t
                                in case break (t1 ==) ps of
                                  (as,_:_) -> ([],as,ts)
                                  -- Unknown closing tag. Insert as text.
                                  (_,[]) ->
                                    let (es,qs,ts1) = nodes ns ps ts
                                    in (Text CData {
                                               cdLine = Just p,
                                               cdVerbatim = CDataText,
                                               cdData = tagEnd t ""
                                              } : es,qs, ts1)

nodes _ ps []                 = ([],ps,[])


annotName :: NSInfo -> QName -> QName
annotName (namespaces,def_ns) n =
  n { qURI = maybe def_ns (`lookup` namespaces) (qPrefix n) }

annotAttr :: NSInfo -> Attr -> Attr
annotAttr ns a@(Attr { attrKey = k}) =
  case (qPrefix k, qName k) of
    -- Do not apply the default name-space to unqualified
    -- attributes.  See Section 6.2 of <http://www.w3.org/TR/REC-xml-names>.
    (Nothing, _)      -> a
    _                 -> a { attrKey = annotName ns k }

addNS :: Attr -> NSInfo -> NSInfo
addNS (Attr { attrKey = key, attrVal = val }) (ns,def) =
  case (qPrefix key, qName key) of
    (Nothing,"xmlns") -> (ns, if null val then Nothing else Just val)
    (Just "xmlns", k) -> ((k, val) : ns, def)
    _                 -> (ns,def)
