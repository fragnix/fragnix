{-# LINE 1 "./Text/XML/Light/Lexer.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                          






                                 






                          






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Text/XML/Light/Lexer.hs" #-}
{-# LINE 1 "./Text/XML/Light/Lexer.hs" #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Text.XML.Light.Lexer where

import Text.XML.Light.Types

import Data.Char (chr,isSpace)
import Numeric (readHex)
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as TS
import qualified Data.Text.Lazy       as TL


class XmlSource s where
  uncons :: s -> Maybe (Char,s)

instance XmlSource String where
  uncons (c:s) = Just (c,s)
  uncons ""    = Nothing

instance XmlSource S.ByteString where
  uncons bs = f `fmap` S.uncons bs
    where f (c,s) = (chr (fromEnum c), s)

instance XmlSource L.ByteString where
  uncons bs = f `fmap` L.uncons bs
    where f (c,s) = (chr (fromEnum c), s)

instance XmlSource TS.Text where
  uncons = TS.uncons

instance XmlSource TL.Text where
  uncons = TL.uncons

linenumber :: XmlSource s => Integer -> s -> LString
linenumber n s = case uncons s of
  Nothing -> []
  Just ('\r', s')   -> case uncons s' of
    Just ('\n',s'') -> next s''
    _               -> next s'
  Just ('\n', s') -> next s'
  Just (c   , s') -> (n,c) : linenumber n s'
  where
  next s' = n' `seq` ((n,'\n'):linenumber n' s') where n' = n + 1


-- | This type may be used to provide a custom scanning function
-- for extracting characters.
data Scanner s = Scanner (Maybe (Char,s)) (s -> Maybe (Char,s))

-- | This type may be used to provide a custom scanning function
-- for extracting characters.
customScanner :: (s -> Maybe (Char,s)) -> s -> Scanner s
customScanner next s = Scanner (next s) next

instance XmlSource (Scanner s) where
  uncons (Scanner this next) = do (c,s1) <- this
                                  return (c, Scanner (next s1) next)


-- Lexer -----------------------------------------------------------------------

type LChar              = (Line,Char)
type LString            = [LChar]
data Token              = TokStart Line QName [Attr] Bool  -- is empty?
                        | TokEnd Line QName
                        | TokCRef String
                        | TokText CData
                          deriving Show

tokens             :: XmlSource source => source -> [Token]
tokens = tokens' . linenumber 1

tokens' :: LString -> [Token]
tokens' ((_,'<') : c@(_,'!') : cs) = special c cs

tokens' ((_,'<') : cs)   = tag (dropSpace cs) -- we are being nice here
tokens' [] = []
tokens' cs@((l,_):_) = let (as,bs) = breakn ('<' ==) cs
                       in map cvt (decode_text as) ++ tokens' bs

  -- XXX: Note, some of the lines might be a bit inacuarate
  where cvt (TxtBit x)  = TokText CData { cdLine = Just l
                                        , cdVerbatim = CDataText
                                        , cdData = x
                                        }
        cvt (CRefBit x) = case cref_to_char x of
                            Just c -> TokText CData { cdLine = Just l
                                                    , cdVerbatim = CDataText
                                                    , cdData = [c]
                                                    }
                            Nothing -> TokCRef x


special :: LChar -> LString -> [Token]
special _ ((_,'-') : (_,'-') : cs) = skip cs
  where skip ((_,'-') : (_,'-') : (_,'>') : ds) = tokens' ds
        skip (_ : ds) = skip ds
        skip [] = [] -- unterminated comment

special c ((_,'[') : (_,'C') : (_,'D') : (_,'A') : (_,'T') : (_,'A') : (_,'[')
         : cs) =
  let (xs,ts) = cdata cs
  in TokText CData { cdLine = Just (fst c), cdVerbatim = CDataVerbatim, cdData = xs }
                                                                  : tokens' ts
  where cdata ((_,']') : (_,']') : (_,'>') : ds) = ([],ds)
        cdata ((_,d) : ds)  = let (xs,ys) = cdata ds in (d:xs,ys)
        cdata []        = ([],[])

special c cs = 
  let (xs,ts) = munch "" 0 cs
  in TokText CData { cdLine = Just (fst c)
                   , cdVerbatim = CDataRaw
                   , cdData = '<':'!':(reverse xs)
                   } : tokens' ts
  where munch acc nesting ((_,'>') : ds) 
         | nesting == (0::Int) = ('>':acc,ds)
	 | otherwise           = munch ('>':acc) (nesting-1) ds
        munch acc nesting ((_,'<') : ds)
	 = munch ('<':acc) (nesting+1) ds
        munch acc n ((_,x) : ds) = munch (x:acc) n ds
        munch acc _ [] = (acc,[]) -- unterminated DTD markup

--special c cs = tag (c : cs) -- invalid specials are processed as tags


qualName           :: LString -> (QName,LString)
qualName xs         = let (as,bs) = breakn endName xs
                          (q,n)   = case break (':'==) as of
                                      (q1,_:n1) -> (Just q1, n1)
                                      _         -> (Nothing, as)
                      in (QName { qURI = Nothing, qPrefix = q, qName = n }, bs)
  where endName x = isSpace x || x == '=' || x == '>' || x == '/'





tag              :: LString -> [Token]
tag ((p,'/') : cs)    = let (n,ds) = qualName (dropSpace cs)
                        in TokEnd p n : case ds of
                                          (_,'>') : es -> tokens' es
                                          -- tag was not properly closed...
                                          _        -> tokens' ds
tag []            = []
tag cs            = let (n,ds)  = qualName cs
                        (as,b,ts) = attribs (dropSpace ds)
                    in TokStart (fst (head cs)) n as b : ts

attribs          :: LString -> ([Attr], Bool, [Token])
attribs cs        = case cs of
                      (_,'>') : ds -> ([], False, tokens' ds)

                      (_,'/') : ds -> ([], True, case ds of
                                              (_,'>') : es -> tokens' es
                                              -- insert missing >  ...
                                              _ -> tokens' ds)

                      (_,'?') : (_,'>') : ds -> ([], True, tokens' ds)

                      -- doc ended within a tag..
                      []       -> ([],False,[])

                      _        -> let (a,cs1) = attrib cs
                                      (as,b,ts) = attribs cs1
                                  in (a:as,b,ts)

attrib             :: LString -> (Attr,LString)
attrib cs           = let (ks,cs1)  = qualName cs
                          (vs,cs2)  = attr_val (dropSpace cs1)
                      in ((Attr ks (decode_attr vs)),dropSpace cs2)

attr_val           :: LString -> (String,LString)
attr_val ((_,'=') : cs) = string (dropSpace cs)
attr_val cs         = ("",cs)


dropSpace :: LString -> LString
dropSpace = dropWhile (isSpace . snd)

-- | Match the value for an attribute.  For malformed XML we do
-- our best to guess the programmer's intention.
string             :: LString -> (String,LString)
string ((_,'"') : cs)   = break' ('"' ==) cs

-- Allow attributes to be enclosed between ' '.
string ((_,'\'') : cs)  = break' ('\'' ==) cs

-- Allow attributes that are not enclosed by anything.
string cs           = breakn eos cs
  where eos x = isSpace x || x == '>' || x == '/'


break' :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
break' p xs         = let (as,bs) = breakn p xs
                      in (as, case bs of
                                [] -> []
                                _ : cs -> cs)

breakn :: (a -> Bool) -> [(b,a)] -> ([a],[(b,a)])
breakn p l = (map snd as,bs) where (as,bs) = break (p . snd) l



decode_attr :: String -> String
decode_attr cs = concatMap cvt (decode_text cs)
  where cvt (TxtBit x) = x
        cvt (CRefBit x) = case cref_to_char x of
                            Just c -> [c]
                            Nothing -> '&' : x ++ ";"

data Txt = TxtBit String | CRefBit String deriving Show

decode_text :: [Char] -> [Txt]
decode_text xs@('&' : cs) = case break (';' ==) cs of
                              (as,_:bs) -> CRefBit as : decode_text bs
                              _ -> [TxtBit xs]
decode_text []  = []
decode_text cs  = let (as,bs) = break ('&' ==) cs
                  in TxtBit as : decode_text bs

cref_to_char :: [Char] -> Maybe Char
cref_to_char cs = case cs of
  '#' : ds  -> num_esc ds
  "lt"      -> Just '<'
  "gt"      -> Just '>'
  "amp"     -> Just '&'
  "apos"    -> Just '\''
  "quot"    -> Just '"'
  _         -> Nothing

num_esc :: String -> Maybe Char
num_esc cs = case cs of
               'x' : ds -> check (readHex ds)
               _        -> check (reads cs)

  where check [(n,"")]  = cvt_char n
        check _         = Nothing

cvt_char :: Int -> Maybe Char
cvt_char x
  | fromEnum (minBound :: Char) <= x && x <= fromEnum (maxBound::Char)
                = Just (toEnum x)
  | otherwise = Nothing
