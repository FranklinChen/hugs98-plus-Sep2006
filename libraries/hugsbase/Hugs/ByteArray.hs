-- Mutable and immutable byte arrays (identical internally), usable for
-- unboxed arrays, and built from FFI primitives.

module Hugs.ByteArray (
	MutableByteArray,
	newMutableByteArray, readMutableByteArray, writeMutableByteArray,
	ByteArray,
	unsafeFreezeMutableByteArray, thawByteArray, readByteArray
    ) where

import Data.Word		( Word8 )
import Foreign.ForeignPtr	( ForeignPtr,
				  mallocForeignPtrBytes, withForeignPtr )
import Foreign.Marshal.Utils	( copyBytes )
import Foreign.Ptr		( castPtr )
import Foreign.Storable		( Storable( peekElemOff, pokeElemOff ))

import Hugs.IOExts		( unsafeCoerce )
import Hugs.ST			( ST, unsafeRunST )

-- This implementation is based on the principle that the FFI primitives
-- used, though declared as IO actions, actually only manipulate local
-- state, and thus could have been declared in the strict ST monad:
--
--	mallocForeignPtrBytes :: Int -> ST s (STForeignPtr s a)
--	withForeignPtr :: STForeignPtr s a -> (STPtr s a -> ST s b) -> ST s b
--	copyBytes :: STPtr s a -> STPtr s a -> Int -> ST s ()
--	castPtr :: STPtr s a -> STPtr s b
--	peekElemOff :: Storable a => STPtr s a -> Int -> ST s a
--	pokeElemOff :: Storable a => STPtr s a -> Int -> a -> ST s ()
--
-- (where STPtr s and STForeignPtr s are just like Ptr and ForeignPtr,
-- but confined to the region s)
--
-- Since the strict ST monad has the same representation as the IO monad,
-- we are justified in coercing such actions to the ST monad.

-- This conversion may be safely applied to computations that manipulate
-- only local state, but will give a runtime error if the IO action does
-- any concurrency.
specialIOToST :: IO a -> ST s a
specialIOToST = unsafeCoerce

type BytePtr = ForeignPtr Word8

data MutableByteArray s = MutableByteArray !Int !BytePtr

newMutableByteArray :: Int -> ST s (MutableByteArray s)
newMutableByteArray size = do
    fp <- specialIOToST (mallocForeignPtrBytes size)
    return (MutableByteArray size fp)

readMutableByteArray :: Storable e => MutableByteArray s -> Int -> ST s e
readMutableByteArray (MutableByteArray _ fp) i =
    specialIOToST $ withForeignPtr fp $ \a -> peekElemOff (castPtr a) i

writeMutableByteArray :: Storable e => MutableByteArray s -> Int -> e -> ST s ()
writeMutableByteArray (MutableByteArray _ fp) i e =
    specialIOToST $ withForeignPtr fp $ \a -> pokeElemOff (castPtr a) i e

data ByteArray = ByteArray !Int !BytePtr

-- Don't change the MutableByteArray after calling this.
unsafeFreezeMutableByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeMutableByteArray (MutableByteArray size fp) =
    return (ByteArray size fp)

thawByteArray :: ByteArray -> ST s (MutableByteArray s)
thawByteArray (ByteArray size fp) = specialIOToST $ do
    fp' <- mallocForeignPtrBytes size
    withForeignPtr fp $ \p ->
	withForeignPtr fp' $ \p' ->
	copyBytes p' p size
    return (MutableByteArray size fp')

-- This one is safe because ByteArrays are immutable
-- (cf. unsafeFreezeMutableByteArray)
readByteArray :: Storable a => ByteArray -> Int -> a
readByteArray (ByteArray _ fp) i = unsafeRunST $ specialIOToST $
    withForeignPtr fp $ \p -> peekElemOff (castPtr p) i
