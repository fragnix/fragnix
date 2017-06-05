{-# LANGUAGE Haskell2010 #-}
{-# LINE 1 "Data/Word8.hs" #-}














































{-# LANGUAGE CPP #-}

-- | Word8 library to be used with Data.ByteString.
-- All function assumes that 'Word8' is encoded in Latin-1 (ISO-8859-1).
-- All utility functions are supposed to work as if
-- those of 'Data.Char'. Exceptions are described in
-- the function documentations.
--
-- Base library 4.7 (GHC 7.8) or earlier is based on Unicode 6.
-- Base library 4.8 (GHC 7.10) or later is based on Unicode 7.
-- 'isLower', 'isSymbol' and 'isPunctuation' behave differently.

module Data.Word8 (
  -- * Re-exporting
    Word8
  -- * Character classification
  , isControl, isSpace, isLower, isUpper
  , isAlpha, isAlphaNum, isPrint, isDigit, isOctDigit, isHexDigit
  , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator
  -- * Subranges
  , isAscii, isLatin1, isAsciiUpper, isAsciiLower
  -- * Case conversion
  , toUpper, toLower, toTitle
  -- * ASCII charactors
  , _nul, _tab, _lf, _vt, _np, _cr
  , _space, _exclam, _quotedbl, _numbersign, _dollar, _percent, _ampersand, _quotesingle, _parenleft, _parenright, _asterisk, _plus, _comma, _hyphen, _period, _slash
  , _0, _1, _2, _3, _4, _5, _6, _7, _8, _9
  , _colon, _semicolon, _less, _equal, _greater, _question, _at
  , _A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z
  , _bracketleft, _backslash, _bracketright, _circum, _underscore, _grave
  , _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z
  , _braceleft, _bar, _braceright, _tilde, _del
  -- * Some Latin-1 charactors
  , _nbsp
  , _ordfeminine, _softhyphen, _mu, _ordmasculine
  , _s2, _s3, _s1, _1'4, _1'2, _3'4
  , _Agrave, _Odieresis, _Oslash, _Thorn
  , _germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis
  ) where

import Data.Word (Word8)


----------------------------------------------------------------

isControl :: Word8 -> Bool
isControl w = _nul <= w && w <= 0x1f
           || _del <= w && w <= 0x9f

isSpace :: Word8 -> Bool
isSpace w = w == _space
         || w == _tab
         || w == _lf
         || w == _cr
         || w == _np
         || w == _vt
         || w == _nbsp

-- | This function returns 'True' for 170 and 186 in Unicode 6.
--   But it returns 'False' in Unicode 7.
isLower :: Word8 -> Bool
isLower w = isLower' w
         || w == _mu

isLowerCommon :: Word8 -> Bool
isLowerCommon w = isLower' w
         || w == _mu
         || w == _ordfeminine
         || w == _ordmasculine

isLower' :: Word8 -> Bool
isLower' w = isAsciiLower w
          || _germandbls <= w && w <= _odieresis
          || _oslash     <= w && w <= _ydieresis

isUpper :: Word8 -> Bool
isUpper w = isAsciiUpper w
         || _Agrave <= w && w <= _Odieresis
         || _Oslash <= w && w <= _Thorn

isAlpha :: Word8 -> Bool
isAlpha w = isLowerCommon w || isUpper w

isAlphaNum :: Word8 -> Bool
isAlphaNum w = isAlpha w || isNumber w

isPrint :: Word8 -> Bool
isPrint w
  | w == _softhyphen = False
isPrint w = _space <= w && w <= _tilde
         || _nbsp  <= w && w <= _ydieresis

isDigit :: Word8 -> Bool
isDigit w = _0 <= w && w <= _9

isOctDigit :: Word8 -> Bool
isOctDigit w = _0 <= w && w <= _7

isHexDigit :: Word8 -> Bool
isHexDigit w = isDigit w
            || _A <= w && w <= _F
            || _a <= w && w <= _f

isLetter :: Word8 -> Bool
isLetter w = isLowerCommon w || isUpper w

isMark :: Word8 -> Bool
isMark _ = False

isNumber :: Word8 -> Bool
isNumber w = isDigit w
          || w == _s1
          || w == _s2
          || w == _s3
          || w == _1'4
          || w == _1'2
          || w == _3'4

-- | This function returns 'False' for 167 and 182 in Unicode 6.
--   But it returns 'True' in Unicode 7.
isPunctuation :: Word8 -> Bool
isPunctuation w = w `elem` [0x21,0x22,0x23,0x25,0x26,0x27,0x28,0x29,0x2a,0x2c,0x2d,0x2e,0x2f,0x3a,0x3b,0x3f,0x40,0x5b,0x5c,0x5d,0x5f,0x7b,0x7d,0xa1,0xa7,0xab,0xb6,0xb7,0xbb,0xbf]

-- | This function returns 'True' for 167 and 182 in Unicode 6.
--   But it returns 'False' in Unicode 7.
isSymbol :: Word8 -> Bool
isSymbol w = w `elem` [0x24,0x2b,0x3c,0x3d,0x3e,0x5e,0x60,0x7c,0x7e,0xa2,0xa3,0xa4,0xa5,0xa6,0xa8,0xa9,0xac,0xae,0xaf,0xb0,0xb1,0xb4,0xb8,0xd7,0xf7]

isSeparator :: Word8 -> Bool
isSeparator w = w == _space
             || w == _nbsp

----------------------------------------------------------------

isAscii :: Word8 -> Bool
isAscii w = _nul <= w && w <= _del

isLatin1 :: Word8 -> Bool
isLatin1 _ = True

isAsciiUpper :: Word8 -> Bool
isAsciiUpper w = _A <= w && w <= _Z

isAsciiLower :: Word8 -> Bool
isAsciiLower w = _a <= w && w <= _z

----------------------------------------------------------------

-- | Micro sign/mu (0xb5) and small letter Y with diaeresis (0xff) remain the same.
toUpper :: Word8 -> Word8
toUpper w
  | w == _germandbls = w
  | isLower' w       = w - _space
  | otherwise        = w

toLower :: Word8 -> Word8
toLower w
  | isUpper w = w + _space
  | otherwise = w

-- | Micro sign/mu (0xb5) and small letter Y with diaeresis (0xff) remain the same.
toTitle :: Word8 -> Word8
toTitle = toUpper

----------------------------------------------------------------

_nul, _tab, _lf, _vt, _np, _cr :: Word8
_nul = 0x00
_tab = 0x09
_lf  = 0x0a
_vt  = 0x0b
_np  = 0x0c
_cr  = 0x0d

_space, _exclam, _quotedbl, _numbersign, _dollar, _percent, _ampersand, _quotesingle, _parenleft, _parenright, _asterisk, _plus, _comma, _hyphen, _period, _slash :: Word8
_space       = 0x20
_exclam      = 0x21
_quotedbl    = 0x22
_numbersign  = 0x23
_dollar      = 0x24
_percent     = 0x25
_ampersand   = 0x26
_quotesingle = 0x27
_parenleft   = 0x28
_parenright  = 0x29
_asterisk    = 0x2a
_plus        = 0x2b
_comma       = 0x2c
_hyphen      = 0x2d
_period      = 0x2e
_slash       = 0x2f

_0, _1, _2, _3, _4, _5, _6, _7, _8, _9 :: Word8
_0 = 0x30
_1 = 0x31
_2 = 0x32
_3 = 0x33
_4 = 0x34
_5 = 0x35
_6 = 0x36
_7 = 0x37
_8 = 0x38
_9 = 0x39

_colon, _semicolon, _less, _equal, _greater, _question, _at :: Word8
_colon      = 0x3a
_semicolon  = 0x3b
_less       = 0x3c
_equal      = 0x3d
_greater    = 0x3e
_question   = 0x3f
_at         = 0x40

_A, _B, _C, _D, _E, _F, _G, _H, _I, _J, _K, _L, _M, _N, _O, _P, _Q, _R, _S, _T, _U, _V, _W, _X, _Y, _Z :: Word8
_A = 0x41
_B = 0x42
_C = 0x43
_D = 0x44
_E = 0x45
_F = 0x46
_G = 0x47
_H = 0x48
_I = 0x49
_J = 0x4a
_K = 0x4b
_L = 0x4c
_M = 0x4d
_N = 0x4e
_O = 0x4f
_P = 0x50
_Q = 0x51
_R = 0x52
_S = 0x53
_T = 0x54
_U = 0x55
_V = 0x56
_W = 0x57
_X = 0x58
_Y = 0x59
_Z = 0x5a

_bracketleft, _backslash, _bracketright, _circum, _underscore, _grave :: Word8
_bracketleft   = 0x5b
_backslash    = 0x5c
_bracketright = 0x5d
_circum       = 0x5e
_underscore   = 0x5f
_grave        = 0x60

_a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k, _l, _m, _n, _o, _p, _q, _r, _s, _t, _u, _v, _w, _x, _y, _z :: Word8
_a = 0x61
_b = 0x62
_c = 0x63
_d = 0x64
_e = 0x65
_f = 0x66
_g = 0x67
_h = 0x68
_i = 0x69
_j = 0x6a
_k = 0x6b
_l = 0x6c
_m = 0x6d
_n = 0x6e
_o = 0x6f
_p = 0x70
_q = 0x71
_r = 0x72
_s = 0x73
_t = 0x74
_u = 0x75
_v = 0x76
_w = 0x77
_x = 0x78
_y = 0x79
_z = 0x7a

_braceleft, _bar, _braceright, _tilde, _del :: Word8
_braceleft  = 0x7b
_bar        = 0x7c
_braceright = 0x7d
_tilde      = 0x7e
_del        = 0x7f

_nbsp :: Word8
_nbsp = 0xa0

_ordfeminine, _softhyphen, _mu, _ordmasculine :: Word8
_ordfeminine  = 0xaa
_softhyphen   = 0xad
_mu           = 0xb5
_ordmasculine = 0xba

_s2, _s3, _s1, _1'4, _1'2, _3'4  :: Word8
_s2 = 0xb2
_s3 = 0xb3
_s1 = 0xb9
_1'4 = 0xbc
_1'2 = 0xbd
_3'4 = 0xbe

_Agrave, _Odieresis, _Oslash, _Thorn :: Word8
_Agrave    = 0xc0
_Odieresis = 0xd6
_Oslash    = 0xd8
_Thorn     = 0xde

_germandbls, _agrave, _odieresis, _oslash, _thorn, _ydieresis :: Word8
_germandbls = 0xdf
_agrave     = 0xe0
_odieresis  = 0xf6
_oslash     = 0xf8
_thorn      = 0xfe
_ydieresis  = 0xff
