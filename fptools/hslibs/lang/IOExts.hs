module IOExts
  {-# DEPRECATED "This library will go away soon; see Data.Array.IO, Data.IORef, and System.IO" #-} 
  (
   module System.IO.Unsafe,

   IOArray,
   newIOArray,
   boundsIOArray,
   readIOArray,
   writeIOArray,
   freezeIOArray,
   thawIOArray,
   unsafeFreezeIOArray,
   unsafeThawIOArray,

   module Data.IORef,
   System.IO.fixIO,

   IOModeEx(..),
   openFileEx,
   hSetBinaryMode

	, hGetBuf            	-- :: Handle -> Ptr a -> Int -> IO Int
	, hGetBufBA  	      	-- :: Handle -> MutableByteArray RealWorld a 
		      	      	--	-> Int -> IO Int

	, hPutBuf     	      	-- :: Handle -> Ptr a -> Int -> IO ()
	, hPutBufBA   	      	-- :: Handle -> MutableByteArray RealWorld a
		      	      	--	-> Int -> IO ()
	, slurpFile

        , trace		      -- :: String -> a -> a

	, hIsTerminalDevice 	-- :: Handle -> IO Bool
	, hSetEcho		-- :: Handle -> Bool -> IO ()
	, hGetEcho		-- :: Handle -> IO Bool
	
	, performGC

	, hTell                	-- :: Handle -> IO Integer

{-	unsafePtrEq		-- :: a -> a -> Bool
	freeHaskellFunctionPtr
	
	 extended IOError predicates
	isHardwareFault		--  :: IOError -> Bool
	isInappropriateType		--  :: IOError -> Bool
	isInterrupted			--  :: IOError -> Bool
	isInvalidArgument		--  :: IOError -> Bool
	isOtherError			--  :: IOError -> Bool
	isProtocolError		--  :: IOError -> Bool
	isResourceVanished		--  :: IOError -> Bool
	isSystemError			--  :: IOError -> Bool
	isTimeExpired			--  :: IOError -> Bool
	isUnsatisfiedConstraints	--  :: IOError -> Bool
	isUnsupportedOperation	--  :: IOError -> Bool
#if defined(cygwin32_HOST_OS) || defined(mingw32_HOST_OS)
	isComError			--  :: IOError -> Bool
#endif
-}
  ) where

import GHC.IOBase
import System.IO
import System.IO.Unsafe   
import System.Mem	( performGC )
import Data.Array.IO
import Data.IORef
import Debug.Trace	( trace )
import Data.Array	( Array )

import GHC.Base		( RealWorld )
import GHC.IO		( slurpFile, memcpy_ba_baoff, memcpy_baoff_ba )
import GHC.Handle
import MutableArray
import Control.Monad	( liftM )

-- ---------------------------------------------------------------------------
-- IOArray compat.

unsafeThawIOArray :: Ix ix => Array ix elt -> IO (IOArray ix elt)
unsafeThawIOArray = unsafeThaw

unsafeFreezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
unsafeFreezeIOArray = unsafeFreeze

thawIOArray :: Ix ix => Array ix elt -> IO (IOArray ix elt)
thawIOArray = thaw

freezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
freezeIOArray = freeze

boundsIOArray :: Ix ix => IOArray ix elt -> (ix, ix)
boundsIOArray = bounds

-- ---------------------------------------------------------------------------
-- hGetBufBA

hGetBufBA :: Handle -> MutableByteArray RealWorld a -> Int -> IO Int
hGetBufBA handle (MutableByteArray _ _ ptr) count
  | count <= 0 = illegalBufferSize handle "hGetBuf" count
  | otherwise = 
      wantReadableHandle "hGetBuf" handle $ 
	\ handle_@Handle__{ haFD=fd, haBuffer=ref, haIsStream=isStream } -> do
	buf@Buffer{ bufBuf=raw, bufWPtr=w, bufRPtr=r } <- readIORef ref
	if bufferEmpty buf
	   then readChunkBA fd isStream ptr 0 count
	   else do 
		let avail = w - r
		copied <- if (count >= avail)
		       	    then do 
				memcpy_ba_baoff ptr raw r (fromIntegral avail)
				writeIORef ref buf{ bufWPtr=0, bufRPtr=0 }
				return avail
		     	    else do 
				memcpy_ba_baoff ptr raw r (fromIntegral count)
				writeIORef ref buf{ bufRPtr = r + count }
				return count

		let remaining = count - copied
		if remaining > 0 
		   then do rest <- readChunkBA fd isStream ptr copied remaining
			   return (rest + copied)
		   else return count

readChunkBA :: FD -> Bool -> RawBuffer -> Int -> Int -> IO Int
readChunkBA fd is_stream ptr init_off bytes = loop init_off bytes 
 where
  loop :: Int -> Int -> IO Int
  loop off bytes | bytes <= 0 = return (off - init_off)
  loop off bytes = do
    r <- fromIntegral `liftM`
           readRawBuffer "IOExts.readChunk" (fromIntegral fd) is_stream ptr
	   				    (fromIntegral off) (fromIntegral bytes)
    if r == 0
	then return (off - init_off)
	else loop (off + r) (bytes - r)

-- -----------------------------------------------------------------------------
-- hPutBufBA

hPutBufBA
	:: Handle			-- handle to write to
	-> MutableByteArray RealWorld a -- buffer
	-> Int				-- number of bytes of data in buffer
	-> IO ()

hPutBufBA handle (MutableByteArray _ _ raw) count
  | count <= 0 = illegalBufferSize handle "hPutBufBA" count
  | otherwise = do
    wantWritableHandle "hPutBufBA" handle $ 
      \ handle_@Handle__{ haFD=fd, haBuffer=ref } -> do

        old_buf@Buffer{ bufBuf=old_raw, bufWPtr=w, bufSize=size }
	  <- readIORef ref

        -- enough room in handle buffer?
        if (size - w > count)
		-- There's enough room in the buffer:
		-- just copy the data in and update bufWPtr.
	    then do memcpy_baoff_ba old_raw w raw (fromIntegral count)
		    writeIORef ref old_buf{ bufWPtr = w + count }
		    return ()

		-- else, we have to flush
	    else do flushed_buf <- flushWriteBuffer fd (haIsStream handle_) old_buf
		    writeIORef ref flushed_buf
		    let this_buf = 
			    Buffer{ bufBuf=raw, bufState=WriteBuffer, 
				    bufRPtr=0, bufWPtr=count, bufSize=count }
		    flushWriteBuffer fd (haIsStream handle_) this_buf
		    return ()

-----------------------------------------------------------------------------
-- Internal Utils

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn (sz :: Int) = 
	ioException (IOError (Just handle)
			    InvalidArgument  fn
			    ("illegal buffer size " ++ showsPrec 9 sz [])
			    Nothing)


-- -----------------------------------------------------------------------------
-- openFileEx

{-# DEPRECATED openFileEx, IOModeEx "use System.IO.openBinaryFile instead" #-}

data IOModeEx 
 = BinaryMode IOMode
 | TextMode   IOMode
   deriving (Eq, Read, Show)

openFileEx :: FilePath -> IOModeEx -> IO Handle
openFileEx path (TextMode   mode) = openFile path mode
openFileEx path (BinaryMode mode) = openBinaryFile path mode
