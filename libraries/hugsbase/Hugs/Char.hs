module Hugs.Char (
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    intToDigit,
    toUpper, toLower, toTitle,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,
    primUniGenCat
    ) where

import Hugs.Prelude(
    isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    readLitChar, showLitChar, lexLitChar)

-- The Hugs Char type covers only the ISO 8859-1 (Latin-1) subset of Unicode,
-- i.e. '\0' to '\xff'.

-- Character-testing operations (some others are in Hugs.Prelude)
isAscii, isLatin1, isControl :: Char -> Bool

isAscii c                =  c < '\x80'

isLatin1 c               =  c <= '\xff'

isControl c              =  c < ' ' || c >= '\DEL' && c <= '\x9f'

primitive isPrint        :: Char -> Bool

-- Digit conversion operations
intToDigit               :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"

-- Case-changing operations
primitive toUpper        :: Char -> Char
primitive toLower        :: Char -> Char
primitive toTitle        :: Char -> Char

-- Character code functions
ord                      :: Char -> Int
ord                      =  fromEnum

chr                      :: Int  -> Char
chr                      =  toEnum

-- Unicode character classification
primitive primUniGenCat  :: Char -> Int
