{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/Char8.hs" #-}









































{-# LANGUAGE MagicHash, CPP #-}

-- | Char8 library to be used with Data.ByteString.Char8.
-- All function assumes that only 8bit part of 'Char' is used
-- and it is encoded in Latin-1 (ISO-8859-1).
-- All utility functions are supposed to work as if
-- those of 'Data.Char'. Exceptions are described in
-- the function documentations.

module Data.Char8 (
  -- * Character classification
    isControl, isSpace, isLower, isUpper
  , isAlpha, isAlphaNum, isPrint, isDigit, isOctDigit, isHexDigit
  , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator
  -- * Subranges
  , isAscii, isLatin1, isAsciiUpper, isAsciiLower
  -- * Case conversion
  , toUpper, toLower, toTitle
  ) where

import GHC.Base
----------------------------------------------------------------

isControl :: Char -> Bool
isControl c = _nul <= c && c <= '\x1f'
           || _del <= c && c <= '\x9f'

isSpace :: Char -> Bool
isSpace c = c == _space
         || c == _tab
         || c == _lf
         || c == _cr
         || c == _np
         || c == _vt
         || c == _nbsp

isLower :: Char -> Bool
isLower c = isLower' c
         || c == _ordfeminine
         || c == _mu
         || c == _ordmasculine

isLower' :: Char -> Bool
isLower' c = isAsciiLower c
          || _germandbls <= c && c <= _odieresis
          || _oslash     <= c && c <= _ydieresis

isUpper :: Char -> Bool
isUpper c = isAsciiUpper c
         || _Agrave <= c && c <= _Odieresis
         || _Oslash <= c && c <= _Thorn

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isNumber c

isPrint :: Char -> Bool
isPrint c
  | c == _softhyphen = False
isPrint c = _space <= c && c <= '~'
         || _nbsp  <= c && c <= _ydieresis

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isOctDigit :: Char -> Bool
isOctDigit c = '0' <= c && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c
            || 'A' <= c && c <= 'F'
            || 'a' <= c && c <= 'f'

isLetter :: Char -> Bool
isLetter c = isLower c || isUpper c

isMark :: Char -> Bool
isMark _ = False

isNumber :: Char -> Bool
isNumber c = isDigit c
          || c == _s1
          || c == _s2
          || c == _s3
          || c == _1'4
          || c == _1'2
          || c == _3'4

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ['\x21','\x22','\x23','\x25','\x26','\x27','\x28','\x29','\x2a','\x2c','\x2d','\x2e','\x2f','\x3a','\x3b','\x3f','\x40','\x5b','\x5c','\x5d','\x5f','\x7b','\x7d','\xa1','\xab','\xb7','\xbb','\xbf']

isSymbol :: Char -> Bool
isSymbol c = c `elem` ['\x24','\x2b','\x3c','\x3d','\x3e','\x5e','\x60','\x7c','\x7e','\xa2','\xa3','\xa4','\xa5','\xa6','\xa7','\xa8','\xa9','\xac','\xae','\xaf','\xb0','\xb1','\xb4','\xb6','\xb8','\xd7','\xf7']

isSeparator :: Char -> Bool
isSeparator c = c == _space
             || c == _nbsp

----------------------------------------------------------------

isAscii :: Char -> Bool
isAscii c = _nul <= c && c <= _del

isLatin1 :: Char -> Bool
isLatin1 (C# c#) = isTrue# (ord# c# <=# 0xff#)

isAsciiUpper :: Char -> Bool
isAsciiUpper c = 'A' <= c && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = 'a' <= c && c <= 'z'

----------------------------------------------------------------

-- | Micro sign/mu (0xb5) and small letter Y with diaeresis (0xff) remain the same.
toUpper :: Char -> Char
toUpper c@(C# c#)
  | c == _germandbls = c
  | isLower' c       = C# (chr# (ord# c# -# 32#))
  | otherwise        = c

toLower :: Char -> Char
toLower c@(C# c#)
  | isUpper c = C# (chr# (ord# c# +# 32#))
  | otherwise = c

-- | Micro sign/mu (0xb5) and small letter Y with diaeresis (0xff) remain the same.
toTitle :: Char -> Char
toTitle = toUpper

----------------------------------------------------------------

_nul, _tab, _lf, _vt, _np, _cr :: Char
_nul = '\x00'
_tab = '\x09'
_lf  = '\x0a'
_vt  = '\x0b'
_np  = '\x0c'
_cr  = '\x0d'

_space, _del, _nbsp :: Char
_space = '\x20'
_del   = '\x7f'
_nbsp  = '\xa0'

_ordfeminine, _softhyphen, _mu, _ordmasculine :: Char
_ordfeminine  = '\xaa'
_softhyphen   = '\xad'
_mu           = '\xb5'
_ordmasculine = '\xba'

_s2, _s3, _s1, _1'4, _1'2, _3'4  :: Char
_s2 = '\xb2'
_s3 = '\xb3'
_s1 = '\xb9'
_1'4 = '\xbc'
_1'2 = '\xbd'
_3'4 = '\xbe'

_Agrave, _Odieresis, _Oslash, _Thorn :: Char
_Agrave    = '\xc0'
_Odieresis = '\xd6'
_Oslash    = '\xd8'
_Thorn     = '\xde'

_germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis :: Char
_germandbls = '\xdf'
_agrave     = '\xe0'
_odieresis  = '\xf6'
_oslash     = '\xf8'
_thorn      = '\xfe'
_ydieresis  = '\xff'
