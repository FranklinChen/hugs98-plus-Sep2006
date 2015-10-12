%
% (c) The AQUA Project, Glasgow University, 1994-2000
%

\section[Addr]{Module @Addr@}

\begin{code}
{-# OPTIONS -monly-3-regs #-}

#include "MachDeps.h"

module Addr {-# DEPRECATED "This library will go away soon; use Foreign.Ptr instead" #-} ( 
          Addr(..)	-- abstract, instance of Eq, Ord, Show, Typeable
	, nullAddr			-- :: Addr
	, alignAddr			-- :: Addr -> Int   -> Addr
	, plusAddr			-- :: Addr -> Int   -> Addr
	, minusAddr			-- :: Addr -> Addr  -> Int

	-- SUP: deprecated in the new FFI, index/read/write???OffAddr are
	-- subsumed by the Storable class
	-- NOTE: The functions for ForeignObj, StablePtr, and Word have
	-- officially never been part of this module.
	, indexCharOffAddr		-- :: Addr -> Int -> Char
	, indexIntOffAddr		-- :: Addr -> Int -> Int
	, indexAddrOffAddr		-- :: Addr -> Int -> Addr
	, indexFloatOffAddr		-- :: Addr -> Int -> Float
	, indexDoubleOffAddr		-- :: Addr -> Int -> Double
	, indexWord8OffAddr		-- :: Addr -> Int -> Word8
	, indexWord16OffAddr		-- :: Addr -> Int -> Word16
	, indexWord32OffAddr		-- :: Addr -> Int -> Word32
	, indexWord64OffAddr		-- :: Addr -> Int -> Word64
	, indexInt8OffAddr		-- :: Addr -> Int -> Int8
	, indexInt16OffAddr		-- :: Addr -> Int -> Int16
	, indexInt32OffAddr		-- :: Addr -> Int -> Int32
	, indexInt64OffAddr		-- :: Addr -> Int -> Int64
	, indexStablePtrOffAddr		-- :: Addr -> Int -> StablePtr a
	, indexWordOffAddr		-- :: Addr -> Int -> Word


	, readCharOffAddr		-- :: Addr -> Int -> IO Char
	, readIntOffAddr		-- :: Addr -> Int -> IO Int
	, readAddrOffAddr		-- :: Addr -> Int -> IO Addr
	, readFloatOffAddr		-- :: Addr -> Int -> IO Float
	, readDoubleOffAddr		-- :: Addr -> Int -> IO Double
	, readWord8OffAddr		-- :: Addr -> Int -> IO Word8
	, readWord16OffAddr		-- :: Addr -> Int -> IO Word16
	, readWord32OffAddr		-- :: Addr -> Int -> IO Word32
	, readWord64OffAddr		-- :: Addr -> Int -> IO Word64
	, readInt8OffAddr		-- :: Addr -> Int -> IO Int8
	, readInt16OffAddr		-- :: Addr -> Int -> IO Int16
	, readInt32OffAddr		-- :: Addr -> Int -> IO Int32
	, readInt64OffAddr		-- :: Addr -> Int -> IO Int64
	, readStablePtrOffAddr		-- :: Addr -> Int -> IO (StablePtr a)
	, readWordOffAddr		-- :: Addr -> Int -> IO Word

	, writeCharOffAddr		-- :: Addr -> Int -> Char   -> IO ()
	, writeIntOffAddr		-- :: Addr -> Int -> Int    -> IO ()
	, writeAddrOffAddr		-- :: Addr -> Int -> Addr   -> IO ()
	, writeFloatOffAddr		-- :: Addr -> Int -> Float  -> IO ()
	, writeDoubleOffAddr		-- :: Addr -> Int -> Double -> IO ()
	, writeWord8OffAddr		-- :: Addr -> Int -> Word8  -> IO ()
	, writeWord16OffAddr		-- :: Addr -> Int -> Word16 -> IO ()
	, writeWord32OffAddr		-- :: Addr -> Int -> Word32 -> IO ()
	, writeWord64OffAddr		-- :: Addr -> Int -> Word64 -> IO ()
	, writeInt8OffAddr		-- :: Addr -> Int -> Int8   -> IO ()
	, writeInt16OffAddr		-- :: Addr -> Int -> Int16  -> IO ()
	, writeInt32OffAddr		-- :: Addr -> Int -> Int32  -> IO ()
	, writeInt64OffAddr		-- :: Addr -> Int -> Int64  -> IO ()
	, writeStablePtrOffAddr		-- :: Addr -> Int -> StablePtr a -> IO ()
#ifndef __PARALLEL_HASKELL__
--	ForeignObj imports lots of stuff from Addr, so it makes
--	a horrible module loop to export writeForeignObjOffAddr here
--	Just import ForeignObj if you want it
--	, writeForeignObjOffAddr	-- :: Addr -> Int -> ForeignObj -> IO ()
#endif
	, writeWordOffAddr		-- :: Addr -> Int -> Word  -> IO ()

	-- deprecated (non-standard) coercions
#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
	, addrToInt			-- :: Addr -> Int  
	, intToAddr			-- :: Int  -> Addr
#endif

	, Word(..)
	, wordToInt			-- :: Word -> Int
	, intToWord			-- :: Int  -> Word
    
	, addrToPtr                     -- :: Addr -> Ptr a
	, ptrToAddr                     -- :: Ptr a -> Addr
	) where

import GHC.Ptr		( Ptr(..) )
import Numeric		( showHex )
import Foreign

import GHC.Int
import GHC.Word
import GHC.Float	( Float(..), Double(..) )
import GHC.Stable	( StablePtr(..) )
import GHC.Base
import GHC.IOBase	( IO(..) )
import GHC.Num		( Integer(J#) )
\end{code}

\begin{code}
data Addr = A# Addr# 	deriving (Eq, Ord)

nullAddr :: Addr
nullAddr = A# nullAddr#

alignAddr :: Addr -> Int -> Addr
alignAddr addr@(A# a) (I# i)
  = case remAddr# a i of {
      0# -> addr;
      n  -> A# (plusAddr# a (i -# n)) }

plusAddr :: Addr -> Int -> Addr
plusAddr (A# addr) (I# off) = A# (plusAddr# addr off)

minusAddr :: Addr -> Addr -> Int
minusAddr (A# a1) (A# a2) = I# (minusAddr# a1 a2)

#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
instance Show Addr where
   showsPrec p (A# addr) rs = pad_out (showHex (word2Integer(int2Word#(addr2Int# addr))) "") rs
     where
        -- want 0s prefixed to pad it out to a fixed length.
       pad_out ls rs = 
	  '0':'x':(replicate (2*SIZEOF_HSPTR - length ls) '0') ++ ls ++ rs
       -- word2Integer :: Word# -> Integer (stolen from Word.lhs)
       word2Integer w = case word2Integer# w of
			(# s, d #) -> J# s d
#endif

instance Storable Addr where
   sizeOf    _             = SIZEOF_HSPTR
   alignment _             = ALIGNMENT_HSPTR
   peekElemOff (Ptr a) i   = readAddrOffAddr (A# a) i
   pokeElemOff (Ptr a) i x = writeAddrOffAddr (A# a) i x

indexAddrOffAddr   :: Addr -> Int -> Addr
indexAddrOffAddr (A# addr#) n
  = case n  	    	    	    	of { I# n# ->
    case indexAddrOffAddr# addr# n# 	of { r# ->
    (A# r#)}}
\end{code}

Word Type

\begin{code}
{-# DEPRECATED wordToInt "use fromIntegral instead" #-}
wordToInt :: Word -> Int
wordToInt (W# w#) = I# (word2Int# w#)

{-# DEPRECATED intToWord "use fromIntegral instead" #-}
intToWord :: Int -> Word
intToWord (I# i#) = W# (int2Word# i#)
\end{code}

Coercing between machine ints and Addrs (deprecated)

\begin{code}
addrToInt :: Addr -> Int
intToAddr :: Int -> Addr

#if (WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64)
addrToInt (A# a#) = I# (addr2Int# a#)
intToAddr (I# i#) = A# (int2Addr# i#)
#endif
\end{code}

@Addr@ is still around, accommodate conversions to/from Ptrs.
\begin{code}
addrToPtr :: Addr -> Ptr a
addrToPtr (A# a) = Ptr a

ptrToAddr :: Ptr a -> Addr
ptrToAddr (Ptr x) = A# x
\end{code}

Indexing immutable memory:
SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
indexCharOffAddr   :: Addr -> Int -> Char
indexIntOffAddr    :: Addr -> Int -> Int
indexWordOffAddr   :: Addr -> Int -> Word
--in PrelAddr: indexAddrOffAddr   :: Addr -> Int -> Addr
indexFloatOffAddr  :: Addr -> Int -> Float
indexDoubleOffAddr :: Addr -> Int -> Double
indexStablePtrOffAddr :: Addr -> Int -> StablePtr a
indexInt8OffAddr  :: Addr -> Int -> Int8
indexInt16OffAddr  :: Addr -> Int -> Int16
indexInt32OffAddr  :: Addr -> Int -> Int32
indexInt64OffAddr  :: Addr -> Int -> Int64
indexWord8OffAddr  :: Addr -> Int -> Word8
indexWord16OffAddr  :: Addr -> Int -> Word16
indexWord32OffAddr  :: Addr -> Int -> Word32
indexWord64OffAddr  :: Addr -> Int -> Word64

indexCharOffAddr   (A# addr#) (I# n#) = C# (indexCharOffAddr# addr# n#)
indexIntOffAddr    (A# addr#) (I# n#) = I# (indexIntOffAddr# addr# n#)
indexWordOffAddr   (A# addr#) (I# n#) = W# (indexWordOffAddr# addr# n#)
indexFloatOffAddr  (A# addr#) (I# n#) = F# (indexFloatOffAddr# addr# n#)
indexDoubleOffAddr (A# addr#) (I# n#) = D# (indexDoubleOffAddr# addr# n#)
indexStablePtrOffAddr (A# addr#) (I# n#) 
  = StablePtr (indexStablePtrOffAddr# addr# n#)
indexInt8OffAddr (A# a#) (I# i#) = I8# (indexInt8OffAddr# a# i#)
indexInt16OffAddr (A# a#) (I# i#) = I16# (indexInt16OffAddr# a# i#)
indexInt32OffAddr (A# a#) (I# i#) = I32# (indexInt32OffAddr# a# i#)
indexInt64OffAddr (A# a#) (I# i#) = I64# (indexInt64OffAddr# a# i#)

indexWord8OffAddr (A# a#) (I# i#) = W8# (indexWord8OffAddr# a# i#)
indexWord16OffAddr (A# a#) (I# i#) = W16# (indexWord16OffAddr# a# i#)
indexWord32OffAddr (A# a#) (I# i#) = W32# (indexWord32OffAddr# a# i#)
indexWord64OffAddr (A# a#) (I# i#) = W64# (indexWord64OffAddr# a# i#)

\end{code}

Indexing mutable memory:
SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
readCharOffAddr       :: Addr -> Int -> IO Char
readIntOffAddr        :: Addr -> Int -> IO Int
readWordOffAddr       :: Addr -> Int -> IO Word
readAddrOffAddr       :: Addr -> Int -> IO Addr
readFloatOffAddr      :: Addr -> Int -> IO Float
readDoubleOffAddr     :: Addr -> Int -> IO Double
readStablePtrOffAddr  :: Addr -> Int -> IO (StablePtr a)
readInt8OffAddr	      :: Addr -> Int -> IO Int8
readInt16OffAddr      :: Addr -> Int -> IO Int16
readInt32OffAddr      :: Addr -> Int -> IO Int32
readInt64OffAddr      :: Addr -> Int -> IO Int64
readWord8OffAddr      :: Addr -> Int -> IO Word8
readWord16OffAddr     :: Addr -> Int -> IO Word16
readWord32OffAddr     :: Addr -> Int -> IO Word32
readWord64OffAddr     :: Addr -> Int -> IO Word64

readCharOffAddr (A# a) (I# i)
  = IO $ \s -> case readCharOffAddr# a i s       of { (# s,x #) -> (# s, C# x #) }
readIntOffAddr (A# a) (I# i)
  = IO $ \s -> case readIntOffAddr# a i s        of { (# s,x #) -> (# s, I# x #) }
readWordOffAddr (A# a) (I# i)
  = IO $ \s -> case readWordOffAddr# a i s       of { (# s,x #) -> (# s, W# x #) }
readAddrOffAddr (A# a) (I# i)
  = IO $ \s -> case readAddrOffAddr# a i s       of { (# s,x #) -> (# s, A# x #) }
readFloatOffAddr (A# a) (I# i)
  = IO $ \s -> case readFloatOffAddr# a i s      of { (# s,x #) -> (# s, F# x #) }
readDoubleOffAddr (A# a) (I# i)
  = IO $ \s -> case readDoubleOffAddr# a i s     of { (# s,x #) -> (# s, D# x #) }
readStablePtrOffAddr (A# a) (I# i)
  = IO $ \s -> case readStablePtrOffAddr# a i s  of { (# s,x #) -> (# s, StablePtr x #) }

readInt8OffAddr (A# a) (I# i)
  = IO $ \s -> case readInt8OffAddr# a i s of (# s, w #) -> (# s, I8# w #)

readInt16OffAddr (A# a) (I# i)
  = IO $ \s -> case readInt16OffAddr# a i s of (# s, w #) -> (# s, I16# w #)

readInt32OffAddr (A# a) (I# i)
  = IO $ \s -> case readInt32OffAddr# a i s of (# s, w #) -> (# s, I32# w #)

readInt64OffAddr (A# a) (I# i)
  = IO $ \s -> case readInt64OffAddr# a i s of (# s, w #) -> (# s, I64# w #)


readWord8OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord8OffAddr# a i s of (# s, w #) -> (# s, W8# w #)

readWord16OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord16OffAddr# a i s of (# s, w #) -> (# s, W16# w #)

readWord32OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord32OffAddr# a i s of (# s, w #) -> (# s, W32# w #)

readWord64OffAddr (A# a) (I# i)
  = IO $ \s -> case readWord64OffAddr# a i s of (# s, w #) -> (# s, W64# w #)

\end{code}

SUP: deprecated in the new FFI, subsumed by the Storable class

\begin{code}
writeCharOffAddr      :: Addr -> Int -> Char        -> IO ()
writeIntOffAddr       :: Addr -> Int -> Int         -> IO ()
writeWordOffAddr      :: Addr -> Int -> Word        -> IO ()
writeAddrOffAddr      :: Addr -> Int -> Addr        -> IO ()
writeFloatOffAddr     :: Addr -> Int -> Float       -> IO ()
writeDoubleOffAddr    :: Addr -> Int -> Double      -> IO ()
writeStablePtrOffAddr :: Addr -> Int -> StablePtr a -> IO ()
writeInt8OffAddr      :: Addr -> Int -> Int8  -> IO ()
writeInt16OffAddr     :: Addr -> Int -> Int16  -> IO ()
writeInt32OffAddr     :: Addr -> Int -> Int32  -> IO ()
writeInt64OffAddr     :: Addr -> Int -> Int64 -> IO ()
writeWord8OffAddr     :: Addr -> Int -> Word8  -> IO ()
writeWord16OffAddr    :: Addr -> Int -> Word16  -> IO ()
writeWord32OffAddr    :: Addr -> Int -> Word32  -> IO ()
writeWord64OffAddr    :: Addr -> Int -> Word64 -> IO ()

writeCharOffAddr (A# a#) (I# i#) (C# c#) = IO $ \ s# ->
      case (writeCharOffAddr#  a# i# c# s#) of s2# -> (# s2#, () #)

writeIntOffAddr (A# a#) (I# i#) (I# e#) = IO $ \ s# ->
      case (writeIntOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeWordOffAddr (A# a#) (I# i#) (W# e#) = IO $ \ s# ->
      case (writeWordOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeAddrOffAddr (A# a#) (I# i#) (A# e#) = IO $ \ s# ->
      case (writeAddrOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeFloatOffAddr (A# a#) (I# i#) (F# e#) = IO $ \ s# ->
      case (writeFloatOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeDoubleOffAddr (A# a#) (I# i#) (D# e#) = IO $ \ s# ->
      case (writeDoubleOffAddr#  a# i# e# s#) of s2# -> (# s2#, () #)

writeStablePtrOffAddr (A# a#) (I# i#) (StablePtr e#) = IO $ \ s# ->
      case (writeStablePtrOffAddr#  a# i# e# s#) of s2# -> (# s2# , () #)

writeInt8OffAddr (A# a#) (I# i#) (I8# w#) = IO $ \ s# ->
      case (writeInt8OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeInt16OffAddr (A# a#) (I# i#) (I16# w#) = IO $ \ s# ->
      case (writeInt16OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeInt32OffAddr (A# a#) (I# i#) (I32# w#) = IO $ \ s# ->
      case (writeInt32OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeInt64OffAddr (A# a#) (I# i#) (I64# w#) = IO $ \ s# ->
      case (writeInt64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)

writeWord8OffAddr (A# a#) (I# i#) (W8# w#) = IO $ \ s# ->
      case (writeWord8OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord16OffAddr (A# a#) (I# i#) (W16# w#) = IO $ \ s# ->
      case (writeWord16OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord32OffAddr (A# a#) (I# i#) (W32# w#) = IO $ \ s# ->
      case (writeWord32OffAddr# a# i# w# s#) of s2# -> (# s2#, () #)

writeWord64OffAddr (A# a#) (I# i#) (W64# w#) = IO $ \ s# ->
      case (writeWord64OffAddr#  a# i# w# s#) of s2# -> (# s2#, () #)
\end{code}
