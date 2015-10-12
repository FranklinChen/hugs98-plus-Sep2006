-----------------------------------------------------------------------------
-- Unsigned Integers
-- Suitable for use with Hugs 98 on 32 bit systems.
-----------------------------------------------------------------------------
module Hugs.Word
	( Word
	, Word8
	, Word16
	, Word32
	, Word64
	) where

import Hugs.Prelude ( Word, Word8, Word16, Word32, Word64,
                      boundedSucc, boundedPred,
		      boundedEnumFrom, boundedEnumFromTo,
		      boundedEnumFromThen, boundedEnumFromThenTo )
import Hugs.Prelude ( Ix(..) )
import Hugs.Prelude ( (%) )
import Hugs.Prelude ( readDec )
import Hugs.Prelude ( Num(fromInt), Integral(toInt) )
import Hugs.Numeric ( showInt )
import Data.Bits
import Data.Int

-----------------------------------------------------------------------------
-- Word
-----------------------------------------------------------------------------

instance Eq  Word     where (==)    = primEqWord
instance Ord Word     where compare = primCmpWord

instance Num Word where
    (+)           = primPlusWord
    (-)           = primMinusWord
    negate        = primNegateWord
    (*)           = primMulWord
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToWord
    fromInt       = primIntToWord

instance Bounded Word where
    minBound = 0
    maxBound = primMaxWord

instance Real Word where
    toRational x = toInteger x % 1

instance Integral Word where
    div       = primDivWord
    quot      = primQuotWord
    rem       = primRemWord
    mod       = primModWord
    quotRem   = primQrmWord
    divMod    = quotRem
    toInteger = primWordToInteger
    toInt     = primWordToInt

instance Ix Word where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word where
    succ           = boundedSucc
    pred           = boundedPred
    toEnum         = primIntToWord
    fromEnum       = primWordToInt

    enumFrom       = boundedEnumFrom
    enumFromTo     = boundedEnumFromTo
    enumFromThen   = boundedEnumFromThen
    enumFromThenTo = boundedEnumFromThenTo

instance Read Word where
    readsPrec p   = readDec

instance Show Word where
    showsPrec p   = showInt  -- a particularily counterintuitive name!

instance Bits Word where
    (.&.)         = primAndWord
    (.|.)         = primOrWord
    xor           = primXorWord
    complement    = primComplementWord
    shift         = primShiftWord
    rotate        = primRotateWord (bitSize (0::Word))
    bit           = primBitWord
    setBit x i    = x .|. bit i
    clearBit x i  = x .&. complement (bit i)
    complementBit x i = x `xor` bit i
    testBit       = primTestWord
    bitSize  _    = bitSize (0::Int)
    isSigned _    = False

-----------------------------------------------------------------------------
-- Word8
-----------------------------------------------------------------------------

instance Eq  Word8     where (==)    = binop (==)
instance Ord Word8     where compare = binop compare

instance Num Word8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . primIntegerToWord
    fromInt       = to . primIntToWord

instance Bounded Word8 where
    minBound = 0
    maxBound = 0xff

instance Real Word8 where
    toRational x = toInteger x % 1

instance Integral Word8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    toInt         = toInt . from

instance Ix Word8 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    succ          = boundedSucc
    pred          = boundedPred
    toEnum        = fromInt
    fromEnum      = toInt
    enumFrom      = boundedEnumFrom
    enumFromThen  = boundedEnumFromThen

instance Read Word8 where
    readsPrec p   = readDec

instance Show Word8 where
    showsPrec p   = showInt  -- a particularily counterintuitive name!

instance Bits Word8 where
    x .&. y       = to (binop (.&.) x y)
    x .|. y       = to (binop (.|.) x y)
    x `xor` y     = to (binop xor x y)
    complement    = to . complement . from
    x `shift` i   = to (from x `shift` i)
    x `rotate` i  = to (from x `rot` i)
      where rot = primRotateWord 8
    bit           = to . bit
    setBit x i    = to (setBit (from x) i)
    clearBit x i  = to (clearBit (from x) i)
    complementBit x i = to (complementBit (from x) i)
    testBit x i   = testBit (from x) i
    bitSize  _    = 8
    isSigned _    = False

-----------------------------------------------------------------------------
-- Word16
-----------------------------------------------------------------------------

instance Eq  Word16     where (==)    = binop (==)
instance Ord Word16     where compare = binop compare

instance Num Word16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . primIntegerToWord
    fromInt       = to . primIntToWord

instance Bounded Word16 where
    minBound = 0
    maxBound = 0xffff

instance Real Word16 where
    toRational x  = toInteger x % 1

instance Integral Word16 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    toInt         = toInt . from

instance Ix Word16 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
    succ          = boundedSucc
    pred          = boundedPred
    toEnum        = fromInt
    fromEnum      = toInt
    enumFrom      = boundedEnumFrom
    enumFromThen  = boundedEnumFromThen

instance Read Word16 where
    readsPrec p   = readDec

instance Show Word16 where
    showsPrec p   = showInt  -- a particularily counterintuitive name!

instance Bits Word16 where
    x .&. y       = to (binop (.&.) x y)
    x .|. y       = to (binop (.|.) x y)
    x `xor` y     = to (binop xor x y)
    complement    = to . complement . from
    x `shift` i   = to (from x `shift` i)
    x `rotate` i  = to (from x `rot` i)
      where rot = primRotateWord 16
    bit           = to . bit
    setBit x i    = to (setBit (from x) i)
    clearBit x i  = to (clearBit (from x) i)
    complementBit x i = to (complementBit (from x) i)
    testBit x i   = testBit (from x) i
    bitSize  _    = 16
    isSigned _    = False

-----------------------------------------------------------------------------
-- Word32
-----------------------------------------------------------------------------

instance Eq  Word32     where (==)    = binop (==)
instance Ord Word32     where compare = binop compare

instance Num Word32 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . primIntegerToWord
    fromInt       = to . primIntToWord

instance Bounded Word32 where
    minBound = 0
    maxBound = 0xffffffff

instance Real Word32 where
    toRational x  = toInteger x % 1

instance Integral Word32 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    toInteger     = toInteger . from
    toInt         = toInt . from

instance Ix Word32 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    succ          = boundedSucc
    pred          = boundedPred
    toEnum        = fromInt
    fromEnum      = toInt
    enumFrom      = boundedEnumFrom
    enumFromThen  = boundedEnumFromThen
    enumFromTo    = boundedEnumFromTo
    enumFromThenTo = boundedEnumFromThenTo

instance Read Word32 where
    readsPrec p   = readDec

instance Show Word32 where
    showsPrec p   = showInt  -- a particularily counterintuitive name!

instance Bits Word32 where
    x .&. y       = to (binop (.&.) x y)
    x .|. y       = to (binop (.|.) x y)
    x `xor` y     = to (binop xor x y)
    complement    = to . complement . from
    x `shift` i   = to (from x `shift` i)
    x `rotate` i  = to (from x `rot` i)
      where rot = primRotateWord 32
    bit           = to . bit
    setBit x i    = to (setBit (from x) i)
    clearBit x i  = to (clearBit (from x) i)
    complementBit x i = to (complementBit (from x) i)
    testBit x i   = testBit (from x) i
    bitSize  _    = 32
    isSigned _    = False

-----------------------------------------------------------------------------
-- Word64
-----------------------------------------------------------------------------

primitive word64ToWord32 "primWord64ToWord32" :: Word64 -> (Word32,Word32)
primitive word32ToWord64 "primWord32ToWord64" :: Word32 -> Word32 -> Word64

integerToW64 :: Integer -> Word64
integerToW64 x = case x `divMod` 0x100000000 of
	(hi,lo) -> word32ToWord64 (fromInteger hi) (fromInteger lo)

w64ToInteger :: Word64 -> Integer
w64ToInteger x = case word64ToWord32 x of
	(hi,lo) -> toInteger hi * 0x100000000 + toInteger lo

instance Eq Word64 where
    x == y = word64ToWord32 x == word64ToWord32 y

instance Ord Word64 where
    compare x y = compare (word64ToWord32 x) (word64ToWord32 y)

instance Bounded Word64 where
    minBound = word32ToWord64 minBound minBound
    maxBound = word32ToWord64 maxBound maxBound

instance Show Word64 where
    showsPrec p = showInt . toInteger

instance Read Word64 where
    readsPrec p s = [ (fromInteger x,r) | (x,r) <- readDec s ]

instance Num Word64 where
    x + y         = fromInteger (toInteger x + toInteger y)
    x - y         = fromInteger (toInteger x - toInteger y)
    x * y         = fromInteger (toInteger x * toInteger y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = integerToW64

instance Real Word64 where
    toRational x = toInteger x % 1

instance Ix Word64 where
    range (m,n)          = [m..n]
    unsafeIndex (m,_) i  = toInt (i - m)
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word64 where
    succ             = boundedSucc
    pred             = boundedPred
    toEnum           = fromInt
    fromEnum         = toInt

    enumFrom x       = enumFromTo x maxBound
    enumFromTo x y   = map fromInteger [toInteger x .. toInteger y]
    enumFromThen     = boundedEnumFromThen
    enumFromThenTo x y z =
                       map fromInteger [toInteger x, toInteger y .. toInteger z]

instance Integral Word64 where
    x `quotRem` y = (fromInteger q, fromInteger r)
	where (q,r) = toInteger x `quotRem` toInteger y
    toInteger     = w64ToInteger

instance Bits Word64 where
    x .&. y       = liftBinary (.&.) x y
    x .|. y       = liftBinary (.|.) x y
    x `xor` y     = liftBinary xor x y
    complement    = liftUnary complement
    x `shift` i   = fromInteger ((toInteger x `shift` i) `mod`
				 0x10000000000000000)
    x `rotate` i  | i<0  = (x `shift` i) .|. (x `shift` (i+bitSize x))
		  | i==0 = x
		  | i>0  = (x `shift` i) .|. (x `shift` (i-bitSize x))
    bit i | i `mod` 64 < 32 = word32ToWord64 0 (bit i)
          | otherwise       = word32ToWord64 (bit i) 0
    bitSize  _    = 64
    isSigned _    = False

liftBinary :: (Word32 -> Word32 -> Word32) -> Word64 -> Word64 -> Word64
liftBinary op x y = word32ToWord64 (op xhi yhi) (op xlo ylo)
	where	(xhi,xlo) = word64ToWord32 x
		(yhi,ylo) = word64ToWord32 y

liftUnary :: (Word32 -> Word32) -> Word64 -> Word64
liftUnary op x = word32ToWord64 (op xhi) (op xlo)
	where	(xhi,xlo) = word64ToWord32 x

-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Coercions - used to make the instance declarations more uniform
-----------------------------------------------------------------------------

primitive word8ToWord "primWord8ToWord" :: Word8  -> Word
primitive wordToWord8 "primWordToWord8" :: Word -> Word8

primitive word16ToWord "primWord16ToWord" :: Word16 -> Word
primitive wordToWord16 "primWordToWord16" :: Word -> Word16

primitive word32ToWord "primWord32ToWord" :: Word32 -> Word
primitive wordToWord32 "primWordToWord32" :: Word -> Word32

class Coerce a where
    to   :: Word -> a
    from :: a -> Word

instance Coerce Word8 where
    from = word8ToWord
    to   = wordToWord8

instance Coerce Word16 where
    from = word16ToWord
    to   = wordToWord16

instance Coerce Word32 where
    from = word32ToWord
    to   = wordToWord32

binop :: Coerce word => (Word -> Word -> a) -> (word -> word -> a)
binop op x y = from x `op` from y

to2 :: Coerce word => (Word, Word) -> (word, word)
to2 (x,y) = (to x, to y)

-----------------------------------------------------------------------------
-- primitives
-----------------------------------------------------------------------------

primitive primEqWord        :: Word -> Word -> Bool
primitive primCmpWord       :: Word -> Word -> Ordering
primitive primPlusWord,
	  primMinusWord,
	  primMulWord	    :: Word -> Word -> Word
primitive primNegateWord    :: Word -> Word
primitive primIntToWord     :: Int -> Word
primitive primIntegerToWord :: Integer -> Word
primitive primMaxWord       :: Word
primitive primDivWord,
	  primQuotWord,
	  primRemWord,
	  primModWord       :: Word -> Word -> Word
primitive primQrmWord       :: Word -> Word -> (Word,Word)
primitive primWordToInt     :: Word -> Int
primitive primWordToInteger :: Word -> Integer
primitive primAndWord       :: Word -> Word -> Word
primitive primOrWord        :: Word -> Word -> Word
primitive primXorWord       :: Word -> Word -> Word
primitive primComplementWord:: Word -> Word
primitive primShiftWord     :: Word -> Int -> Word
primitive primRotateWord    :: Int -> Word -> Int -> Word
primitive primBitWord       :: Int -> Word
primitive primTestWord      :: Word -> Int -> Bool

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
