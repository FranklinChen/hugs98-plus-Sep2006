----------------------------------------------------------------------------
-- Signed Integers
-- Suitable for use with Hugs 98 on 32 bit systems.
-----------------------------------------------------------------------------

module Hugs.Int
	( Int8
	, Int16
	, Int32
	, Int64
	-- plus Eq, Ord, Num, Bounded, Real, Integral, Ix, Enum, Read,
	--  Show and Bits instances for each of Int8, Int16 and Int32
	) where

import Hugs.Prelude ( Int8, Int16, Int32, Int64,
		      boundedSucc, boundedPred,
                      boundedEnumFrom, boundedEnumFromTo,
                      boundedEnumFromThen, boundedEnumFromThenTo )
import Hugs.Prelude ( Ix(..) )
import Hugs.Prelude ( (%) )
import Hugs.Prelude ( readDec )
import Hugs.Prelude ( Num(fromInt), Integral(toInt) )
import Hugs.Numeric ( showInt )
import Hugs.Bits
import Data.Bits

-----------------------------------------------------------------------------
-- Int8
-----------------------------------------------------------------------------

primitive int8ToInt "primInt8ToInt" :: Int8 -> Int
primitive intToInt8 "primIntToInt8" :: Int -> Int8

instance Eq  Int8     where (==)    = binop (==)
instance Ord Int8     where compare = binop compare

instance Num Int8 where
    x + y         = intToInt8 (binop (+) x y)
    x - y         = intToInt8 (binop (-) x y)
    negate        = intToInt8 . negate . int8ToInt
    x * y         = intToInt8 (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt8 . fromInteger
    fromInt       = intToInt8

instance Bounded Int8 where
    minBound = 0x80
    maxBound = 0x7f

instance Real Int8 where
    toRational x = toInteger x % 1

instance Integral Int8 where
    x `div` y     = intToInt8  (binop div x y)
    x `quot` y    = intToInt8  (binop quot x y)
    x `rem` y     = intToInt8  (binop rem x y)
    x `mod` y     = intToInt8  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . int8ToInt
    toInt         = int8ToInt

instance Ix Int8 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int8 where
    succ             = boundedSucc
    pred             = boundedPred
    toEnum           = fromInt
    fromEnum         = toInt
    enumFrom         = boundedEnumFrom
    enumFromThen     = boundedEnumFromThen

instance Read Int8 where
    readsPrec p s = [ (intToInt8 x,r) | (x,r) <- readsPrec p s ]

instance Show Int8 where
    showsPrec p = showsPrec p . int8ToInt

instance Bits Int8 where
  x .&. y       = intToInt8 (binop (.&.) x y)
  x .|. y       = intToInt8 (binop (.|.) x y)
  x `xor` y     = intToInt8 (binop xor x y)
  complement    = intToInt8 . complement . int8ToInt
  x `shift` i   = intToInt8 (int8ToInt x `shift` i)
  rotate        = rotateSigned
  bit           = intToInt8 . bit
  setBit x i    = intToInt8 (setBit (int8ToInt x) i)
  clearBit x i  = intToInt8 (clearBit (int8ToInt x) i)
  complementBit x i = intToInt8 (complementBit (int8ToInt x) i)
  testBit x i   = testBit (int8ToInt x) i
  bitSize  _    = 8
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int16
-----------------------------------------------------------------------------

primitive int16ToInt "primInt16ToInt" :: Int16 -> Int
primitive intToInt16 "primIntToInt16" :: Int -> Int16

instance Eq  Int16     where (==)    = binop (==)
instance Ord Int16     where compare = binop compare

instance Num Int16 where
    x + y         = intToInt16 (binop (+) x y)
    x - y         = intToInt16 (binop (-) x y)
    negate        = intToInt16 . negate . int16ToInt
    x * y         = intToInt16 (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt16 . fromInteger
    fromInt       = intToInt16

instance Bounded Int16 where
    minBound = 0x8000
    maxBound = 0x7fff

instance Real Int16 where
    toRational x = toInteger x % 1

instance Integral Int16 where
    x `div` y     = intToInt16  (binop div x y)
    x `quot` y    = intToInt16  (binop quot x y)
    x `rem` y     = intToInt16  (binop rem x y)
    x `mod` y     = intToInt16  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . int16ToInt
    toInt         = int16ToInt

instance Ix Int16 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int16 where
    succ             = boundedSucc
    pred             = boundedPred
    toEnum           = fromInt
    fromEnum         = toInt
    enumFrom         = boundedEnumFrom
    enumFromThen     = boundedEnumFromThen

instance Read Int16 where
    readsPrec p s = [ (intToInt16 x,r) | (x,r) <- readsPrec p s ]

instance Show Int16 where
    showsPrec p = showsPrec p . int16ToInt

instance Bits Int16 where
  x .&. y       = intToInt16 (binop (.&.) x y)
  x .|. y       = intToInt16 (binop (.|.) x y)
  x `xor` y     = intToInt16 (binop xor x y)
  complement    = intToInt16 . complement . int16ToInt
  x `shift` i   = intToInt16 (int16ToInt x `shift` i)
  rotate        = rotateSigned
  bit           = intToInt16 . bit
  setBit x i    = intToInt16 (setBit (int16ToInt x) i)
  clearBit x i  = intToInt16 (clearBit (int16ToInt x) i)
  complementBit x i = intToInt16 (complementBit (int16ToInt x) i)
  testBit x i   = testBit (int16ToInt x) i
  bitSize  _    = 16
  isSigned _    = True

-----------------------------------------------------------------------------
-- Int32
-----------------------------------------------------------------------------

primitive int32ToInt "primInt32ToInt" :: Int32 -> Int
primitive intToInt32 "primIntToInt32" :: Int -> Int32

instance Eq  Int32 where (==)    = binop (==)
instance Ord Int32 where compare = binop compare

instance Num Int32 where
    x + y         = intToInt32 (binop (+) x y)
    x - y         = intToInt32 (binop (-) x y)
    negate        = intToInt32 . negate . int32ToInt
    x * y         = intToInt32 (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = intToInt32 . fromInteger
    fromInt       = intToInt32

instance Bounded Int32 where
    minBound = intToInt32 minBound
    maxBound = intToInt32 maxBound

instance Real Int32 where
    toRational x = toInteger x % 1

instance Integral Int32 where
    x `div` y     = intToInt32 (binop div x y)
    x `quot` y    = intToInt32 (binop quot x y)
    x `rem` y     = intToInt32 (binop rem x y)
    x `mod` y     = intToInt32 (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    toInteger     = toInteger . int32ToInt
    toInt         = int32ToInt

instance Ix Int32 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int32 where
    succ             = boundedSucc
    pred             = boundedPred
    toEnum           = fromInt
    fromEnum         = toInt
    enumFrom         = boundedEnumFrom
    enumFromThen     = boundedEnumFromThen

instance Read Int32 where
    readsPrec p s = [ (intToInt32 x,r) | (x,r) <- readsPrec p s ]

instance Show Int32 where
    showsPrec p = showsPrec p . int32ToInt

instance Bits Int32 where
    x .&. y       = intToInt32 (binop (.&.) x y)
    x .|. y       = intToInt32 (binop (.|.) x y)
    x `xor` y     = intToInt32 (binop xor x y)
    complement    = intToInt32 . complement . int32ToInt
    x `shift` i   = intToInt32 (int32ToInt x `shift` i)
    rotate        = rotateSigned
    bit           = intToInt32 . bit
    setBit x i    = intToInt32 (setBit (int32ToInt x) i)
    clearBit x i  = intToInt32 (clearBit (int32ToInt x) i)
    complementBit x i = intToInt32 (complementBit (int32ToInt x) i)
    testBit x i   = testBit (int32ToInt x) i
    bitSize  _    = 32
    isSigned _    = True

-----------------------------------------------------------------------------
-- Int64
-----------------------------------------------------------------------------

-- Assume a 2s-complement representation, and that this function
-- separates the top 32 bits from the lower 32.

primitive int64ToInt32 "primInt64ToInt32" :: Int64 -> (Int32,Int32)
primitive int32ToInt64 "primInt32ToInt64" :: Int32 -> Int32 -> Int64

integerToI64 :: Integer -> Int64
integerToI64 x = case x `divMod` 0x100000000 of
    (hi,lo) -> int32ToInt64 (fromInteger hi) (fromInteger lo)

i64ToInteger :: Int64 -> Integer
i64ToInteger x = case int64ToInt32 x of
    (hi,lo) -> (if lo<0 then toInteger hi+1 else toInteger hi)*0x100000000 +
	toInteger lo

instance Eq Int64 where
    x == y = int64ToInt32 x == int64ToInt32 y

instance Ord Int64 where
    compare x y = compare (toInteger x) (toInteger y)

instance Bounded Int64 where
    minBound = int32ToInt64 minBound 0
    maxBound = int32ToInt64 maxBound (-1)

instance Show Int64 where
    showsPrec p = showsPrec p . toInteger

instance Read Int64 where
    readsPrec p s = [ (fromInteger x,r) | (x,r) <- readDec s ]

instance Num Int64 where
    x + y         = fromInteger (toInteger x + toInteger y)
    x - y         = fromInteger (toInteger x - toInteger y)
    x * y         = fromInteger (toInteger x * toInteger y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = integerToI64

instance Real Int64 where
    toRational x = toInteger x % 1

instance Ix Int64 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Int64 where
    succ             = boundedSucc
    pred             = boundedPred
    toEnum           = fromInt
    fromEnum         = toInt

    enumFrom x       = enumFromTo x maxBound
    enumFromTo x y   = map fromInteger [toInteger x .. toInteger y]
    enumFromThen     = boundedEnumFromThen
    enumFromThenTo x y z =
                       map fromInteger [toInteger x, toInteger y .. toInteger z]

instance Integral Int64 where
    x `quotRem` y = (fromInteger q, fromInteger r)
	where (q,r) = toInteger x `quotRem` toInteger y
    toInteger     = i64ToInteger

instance Bits Int64 where
    x .&. y       = liftBinary (.&.) x y
    x .|. y       = liftBinary (.|.) x y
    x `xor` y     = liftBinary xor x y
    complement    = liftUnary complement
    x `shift` i   = fromInteger (toInteger x `shift` i)
    rotate        = rotateSigned
    bit i | i `mod` 64 < 32 = int32ToInt64 0 (bit i)
          | otherwise       = int32ToInt64 (bit i) 0
    bitSize  _    = 64
    isSigned _    = True

liftBinary :: (Int32 -> Int32 -> Int32) -> Int64 -> Int64 -> Int64
liftBinary op x y = int32ToInt64 (op xhi yhi) (op xlo ylo)
	where	(xhi,xlo) = int64ToInt32 x
		(yhi,ylo) = int64ToInt32 y

liftUnary :: (Int32 -> Int32) -> Int64 -> Int64
liftUnary op x = int32ToInt64 (op xhi) (op xlo)
	where	(xhi,xlo) = int64ToInt32 x

rotateSigned :: (Bits a, Ord a) => a -> Int -> a
rotateSigned x i | i<0 && x<0
                        = let left = i+bitSize x in
                          ((x `shift` i) .&. complement ((-1) `shift` left))
                          .|. (x `shift` left)
                 | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
                 | i==0 = x
                 | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))

-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

binop :: Integral int => (Int -> Int -> a) -> (int -> int -> a)
binop op x y = toInt x `op` toInt y

from :: Integral int => int -> Int
from = toInt

to :: Num int => Int -> int
to = fromInt

to2 :: Num int => (Int, Int) -> (int, int)
to2 (x,y) = (fromInt x, fromInt y)

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

-----------------------------------------------------------------------------
-- End
-----------------------------------------------------------------------------
