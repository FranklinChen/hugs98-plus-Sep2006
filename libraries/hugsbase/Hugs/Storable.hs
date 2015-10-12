module Hugs.Storable where

import Hugs.Prelude
import Hugs.Ptr (castPtr)

{-# CFILES Hugs/Storable_aux.c #-}

foreign import ccall unsafe "Storable_aux.h" readIntOffPtr       :: Ptr Int           -> Int -> IO Int
foreign import ccall unsafe "Storable_aux.h" readCharOffPtr      :: Ptr Char          -> Int -> IO Char
foreign import ccall unsafe "Storable_aux.h" readPtrOffPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
foreign import ccall unsafe "Storable_aux.h" readFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
foreign import ccall unsafe "Storable_aux.h" readFloatOffPtr     :: Ptr Float         -> Int -> IO Float
foreign import ccall unsafe "Storable_aux.h" readDoubleOffPtr    :: Ptr Double        -> Int -> IO Double
foreign import ccall unsafe "Storable_aux.h" readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
foreign import ccall unsafe "Storable_aux.h" readInt8OffPtr      :: Ptr Int8          -> Int -> IO Int8
foreign import ccall unsafe "Storable_aux.h" readInt16OffPtr     :: Ptr Int16         -> Int -> IO Int16
foreign import ccall unsafe "Storable_aux.h" readInt32OffPtr     :: Ptr Int32         -> Int -> IO Int32
foreign import ccall unsafe "Storable_aux.h" readInt64OffPtr     :: Ptr Int64         -> Int -> IO Int64
foreign import ccall unsafe "Storable_aux.h" readWord8OffPtr     :: Ptr Word8         -> Int -> IO Word8
foreign import ccall unsafe "Storable_aux.h" readWord16OffPtr    :: Ptr Word16        -> Int -> IO Word16
foreign import ccall unsafe "Storable_aux.h" readWord32OffPtr    :: Ptr Word32        -> Int -> IO Word32
foreign import ccall unsafe "Storable_aux.h" readWord64OffPtr    :: Ptr Word64        -> Int -> IO Word64

foreign import ccall unsafe "Storable_aux.h" writeIntOffPtr       :: Ptr Int           -> Int -> Int         -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeCharOffPtr      :: Ptr Char          -> Int -> Char        -> IO ()
foreign import ccall unsafe "Storable_aux.h" writePtrOffPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeFloatOffPtr     :: Ptr Float         -> Int -> Float       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeDoubleOffPtr    :: Ptr Double        -> Int -> Double      -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeInt8OffPtr      :: Ptr Int8          -> Int -> Int8        -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeInt16OffPtr     :: Ptr Int16         -> Int -> Int16       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeInt32OffPtr     :: Ptr Int32         -> Int -> Int32       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeInt64OffPtr     :: Ptr Int64         -> Int -> Int64       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeWord8OffPtr     :: Ptr Word8         -> Int -> Word8       -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeWord16OffPtr    :: Ptr Word16        -> Int -> Word16      -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeWord32OffPtr    :: Ptr Word32        -> Int -> Word32      -> IO ()
foreign import ccall unsafe "Storable_aux.h" writeWord64OffPtr    :: Ptr Word64        -> Int -> Word64      -> IO ()

-- Special treatment for Word:
-- it's not a foreign type, but we want an instance of Storable.

readWordOffPtr :: Ptr Word -> Int -> IO Word
readWordOffPtr p n = do
	i <- readIntOffPtr (castPtr p) n
	return (primIntToWord i)

writeWordOffPtr :: Ptr Word -> Int -> Word -> IO ()
writeWordOffPtr p n w = writeIntOffPtr (castPtr p) n (primWordToInt w)

primitive primIntToWord :: Int -> Word
primitive primWordToInt :: Word -> Int
