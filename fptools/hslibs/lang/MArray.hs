module MArray
  {-# DEPRECATED "This module has moved to Data.Array.MArray" #-} 
  (
	module Data.Array.MArray,

    castSTUArray, -- :: STUArray s i a -> ST s (STUArray s i b)
    castIOUArray, -- :: IOUArray i a -> IO (IOUArray i b)

    -- Legacy non-overloaded byte array interface
    -- (CharArrays here work on bytes, not Chars!)
    newCharArray,       -- :: Ix i => (i,i) -> ST s (STUArray s i Char)
    newIntArray,        -- :: Ix i => (i,i) -> ST s (STUArray s i Int)
    newWordArray,       -- :: Ix i => (i,i) -> ST s (STUArray s i Word)
    newAddrArray,       -- :: Ix i => (i,i) -> ST s (STUArray s i Addr)
    newFloatArray,      -- :: Ix i => (i,i) -> ST s (STUArray s i Float)
    newDoubleArray,     -- :: Ix i => (i,i) -> ST s (STUArray s i Double)
    newStablePtrArray,  -- :: Ix i => (i,i) -> ST s (STUArray s i (StablePtr a))
    newInt8Array,       -- :: Ix i => (i,i) -> ST s (STUArray s i Int8)
    newInt16Array,      -- :: Ix i => (i,i) -> ST s (STUArray s i Int16)
    newInt32Array,      -- :: Ix i => (i,i) -> ST s (STUArray s i Int32)
    newInt64Array,      -- :: Ix i => (i,i) -> ST s (STUArray s i Int64)
    newWord8Array,      -- :: Ix i => (i,i) -> ST s (STUArray s i Word8)
    newWord16Array,     -- :: Ix i => (i,i) -> ST s (STUArray s i Word16)
    newWord32Array,     -- :: Ix i => (i,i) -> ST s (STUArray s i Word32)
    newWord64Array,     -- :: Ix i => (i,i) -> ST s (STUArray s i Word64)

    readCharArray,       -- :: Ix i => STUArray s i Char          -> i -> ST s Char
    readIntArray,        -- :: Ix i => STUArray s i Int           -> i -> ST s Int
    readWordArray,       -- :: Ix i => STUArray s i Word          -> i -> ST s Word
    readAddrArray,       -- :: Ix i => STUArray s i Addr          -> i -> ST s Addr
    readFloatArray,      -- :: Ix i => STUArray s i Float         -> i -> ST s Float
    readDoubleArray,     -- :: Ix i => STUArray s i Double        -> i -> ST s Double
    readStablePtrArray,  -- :: Ix i => STUArray s i (StablePtr a) -> i -> ST s (StablePtr a)
    readInt8Array,       -- :: Ix i => STUArray s i Int8          -> i -> ST s Int8
    readInt16Array,      -- :: Ix i => STUArray s i Int16         -> i -> ST s Int16
    readInt32Array,      -- :: Ix i => STUArray s i Int32         -> i -> ST s Int32
    readInt64Array,      -- :: Ix i => STUArray s i Int64         -> i -> ST s Int64
    readWord8Array,      -- :: Ix i => STUArray s i Word8         -> i -> ST s Word8
    readWord16Array,     -- :: Ix i => STUArray s i Word16        -> i -> ST s Word16
    readWord32Array,     -- :: Ix i => STUArray s i Word32        -> i -> ST s Word32
    readWord64Array,     -- :: Ix i => STUArray s i Word64        -> i -> ST s Word64

    writeCharArray,      -- :: Ix i => STUArray s i Char          -> i -> Char        -> ST s ()
    writeIntArray,       -- :: Ix i => STUArray s i Int           -> i -> Int         -> ST s ()
    writeWordArray,      -- :: Ix i => STUArray s i Word          -> i -> Word        -> ST s ()
    writeAddrArray,      -- :: Ix i => STUArray s i Addr          -> i -> Addr        -> ST s ()
    writeFloatArray,     -- :: Ix i => STUArray s i Float         -> i -> Float       -> ST s ()
    writeDoubleArray,    -- :: Ix i => STUArray s i Double        -> i -> Double      -> ST s ()
    writeStablePtrArray, -- :: Ix i => STUArray s i (StablePtr a) -> i -> StablePtr a -> ST s ()
    writeInt8Array,      -- :: Ix i => STUArray s i Int8          -> i -> Int8        -> ST s ()
    writeInt16Array,     -- :: Ix i => STUArray s i Int16         -> i -> Int16       -> ST s ()
    writeInt32Array,     -- :: Ix i => STUArray s i Int32         -> i -> Int32       -> ST s ()
    writeInt64Array,     -- :: Ix i => STUArray s i Int64         -> i -> Int64       -> ST s ()
    writeWord8Array,     -- :: Ix i => STUArray s i Word8         -> i -> Word8       -> ST s ()
    writeWord16Array,    -- :: Ix i => STUArray s i Word16        -> i -> Word16      -> ST s ()
    writeWord32Array,    -- :: Ix i => STUArray s i Word32        -> i -> Word32      -> ST s ()
    writeWord64Array     -- :: Ix i => STUArray s i Word64        -> i -> Word64      -> ST s ()
	) where

import Data.Array.MArray
import Data.Array.IO
import Data.Array.ST
import Control.Monad.ST
import Data.Int
import Data.Word
import Data.Array.Base	( STUArray(..), MArray(..) )
import Addr
import Foreign.StablePtr

import GHC.Exts	 ( Int(I#), Char(C#) )
import GHC.ST	 ( ST(..) )
import GHC.Prim	 ( newByteArray#, readCharArray#, writeCharArray#,
		   readAddrArray#, writeAddrArray#, (*#) )

#include "MachDeps.h"

-----------------------------------------------------------------------------
-- Addr instances

instance MArray (STUArray s) Addr (ST s) where
    {-# INLINE newArray_ #-}
    newArray_ (l,u) = ST $ \s1# ->
        case rangeSize (l,u)            of { I# n# ->
        case newByteArray# (wORD_SCALE n#) s1# of { (# s2#, marr# #) ->
        (# s2#, STUArray l u marr# #) }}
    {-# INLINE unsafeRead #-}
    unsafeRead (STUArray _ _ marr#) (I# i#) = ST $ \s1# ->
        case readAddrArray# marr# i# s1# of { (# s2#, e# #) ->
        (# s2#, A# e# #) }
    {-# INLINE unsafeWrite #-}
    unsafeWrite (STUArray _ _ marr#) (I# i#) (A# e#) = ST $ \s1# ->
        case writeAddrArray# marr# i# e# s1# of { s2# ->
        (# s2#, () #) }

wORD_SCALE   n# = scale# *# n# where I# scale# = SIZEOF_HSWORD

-----------------------------------------------------------------------------
-- Legacy non-overloaded byte array interface

newCharArray      :: Ix i => (i,i) -> ST s (STUArray s i Char)
newIntArray       :: Ix i => (i,i) -> ST s (STUArray s i Int)
newWordArray      :: Ix i => (i,i) -> ST s (STUArray s i Word)
newAddrArray      :: Ix i => (i,i) -> ST s (STUArray s i Addr)
newFloatArray     :: Ix i => (i,i) -> ST s (STUArray s i Float)
newDoubleArray    :: Ix i => (i,i) -> ST s (STUArray s i Double)
newStablePtrArray :: Ix i => (i,i) -> ST s (STUArray s i (StablePtr a))
newInt8Array      :: Ix i => (i,i) -> ST s (STUArray s i Int8)
newInt16Array     :: Ix i => (i,i) -> ST s (STUArray s i Int16)
newInt32Array     :: Ix i => (i,i) -> ST s (STUArray s i Int32)
newInt64Array     :: Ix i => (i,i) -> ST s (STUArray s i Int64)
newWord8Array     :: Ix i => (i,i) -> ST s (STUArray s i Word8)
newWord16Array    :: Ix i => (i,i) -> ST s (STUArray s i Word16)
newWord32Array    :: Ix i => (i,i) -> ST s (STUArray s i Word32)
newWord64Array    :: Ix i => (i,i) -> ST s (STUArray s i Word64)

newCharArray (l,u) = ST $ \s1# ->
    case rangeSize (l,u)                of { I# n# ->
    case newByteArray# n# s1#           of { (# s2#, marr# #) ->
    (# s2#, STUArray l u marr# #) }}
newIntArray       = newArray_
newWordArray      = newArray_
newAddrArray      = newArray_
newFloatArray     = newArray_
newDoubleArray    = newArray_
newStablePtrArray = newArray_
newInt8Array      = newArray_
newInt16Array     = newArray_
newInt32Array     = newArray_
newInt64Array     = newArray_
newWord8Array     = newArray_
newWord16Array    = newArray_
newWord32Array    = newArray_
newWord64Array    = newArray_

readCharArray      :: Ix i => STUArray s i Char          -> i -> ST s Char
readIntArray       :: Ix i => STUArray s i Int           -> i -> ST s Int
readWordArray      :: Ix i => STUArray s i Word          -> i -> ST s Word
readAddrArray      :: Ix i => STUArray s i Addr          -> i -> ST s Addr
readFloatArray     :: Ix i => STUArray s i Float         -> i -> ST s Float
readDoubleArray    :: Ix i => STUArray s i Double        -> i -> ST s Double
readStablePtrArray :: Ix i => STUArray s i (StablePtr a) -> i -> ST s (StablePtr a)
readInt8Array      :: Ix i => STUArray s i Int8          -> i -> ST s Int8
readInt16Array     :: Ix i => STUArray s i Int16         -> i -> ST s Int16
readInt32Array     :: Ix i => STUArray s i Int32         -> i -> ST s Int32
readInt64Array     :: Ix i => STUArray s i Int64         -> i -> ST s Int64
readWord8Array     :: Ix i => STUArray s i Word8         -> i -> ST s Word8
readWord16Array    :: Ix i => STUArray s i Word16        -> i -> ST s Word16
readWord32Array    :: Ix i => STUArray s i Word32        -> i -> ST s Word32
readWord64Array    :: Ix i => STUArray s i Word64        -> i -> ST s Word64

readCharArray (STUArray l u marr#) i = ST $ \s1# ->
    case index (l,u) i                  of { I# i# ->
    case readCharArray# marr# i# s1#    of { (# s2#, e# #) ->
    (# s2#, C# e# #) }}
readIntArray       = readArray
readWordArray      = readArray
readAddrArray      = readArray
readFloatArray     = readArray
readDoubleArray    = readArray
readStablePtrArray = readArray
readInt8Array      = readArray
readInt16Array     = readArray
readInt32Array     = readArray
readInt64Array     = readArray
readWord8Array     = readArray
readWord16Array    = readArray
readWord32Array    = readArray
readWord64Array    = readArray

writeCharArray      :: Ix i => STUArray s i Char          -> i -> Char        -> ST s ()
writeIntArray       :: Ix i => STUArray s i Int           -> i -> Int         -> ST s ()
writeWordArray      :: Ix i => STUArray s i Word          -> i -> Word        -> ST s ()
writeAddrArray      :: Ix i => STUArray s i Addr          -> i -> Addr        -> ST s ()
writeFloatArray     :: Ix i => STUArray s i Float         -> i -> Float       -> ST s ()
writeDoubleArray    :: Ix i => STUArray s i Double        -> i -> Double      -> ST s ()
writeStablePtrArray :: Ix i => STUArray s i (StablePtr a) -> i -> StablePtr a -> ST s ()
writeInt8Array      :: Ix i => STUArray s i Int8          -> i -> Int8        -> ST s ()
writeInt16Array     :: Ix i => STUArray s i Int16         -> i -> Int16       -> ST s ()
writeInt32Array     :: Ix i => STUArray s i Int32         -> i -> Int32       -> ST s ()
writeInt64Array     :: Ix i => STUArray s i Int64         -> i -> Int64       -> ST s ()
writeWord8Array     :: Ix i => STUArray s i Word8         -> i -> Word8       -> ST s ()
writeWord16Array    :: Ix i => STUArray s i Word16        -> i -> Word16      -> ST s ()
writeWord32Array    :: Ix i => STUArray s i Word32        -> i -> Word32      -> ST s ()
writeWord64Array    :: Ix i => STUArray s i Word64        -> i -> Word64      -> ST s ()

writeCharArray (STUArray l u marr#) i (C# e#) = ST $ \s1# ->
    case index (l,u) i                  of { I# i# ->
    case writeCharArray# marr# i# e# s1# of { s2# ->
    (# s2#, () #) }}
writeIntArray       = writeArray
writeWordArray      = writeArray
writeAddrArray      = writeArray
writeFloatArray     = writeArray
writeDoubleArray    = writeArray
writeStablePtrArray = writeArray
writeInt8Array      = writeArray
writeInt16Array     = writeArray
writeInt32Array     = writeArray
writeInt64Array     = writeArray
writeWord8Array     = writeArray
writeWord16Array    = writeArray
writeWord32Array    = writeArray
writeWord64Array    = writeArray
