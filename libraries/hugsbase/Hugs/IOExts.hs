-----------------------------------------------------------------------------
-- IO monad extensions:
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Hugs.IOExts
	( unsafePerformIO	-- :: IO a -> a
	, unsafeInterleaveIO	-- :: IO a -> IO a

	, performGC

	, IOModeEx(..)	      	-- instance (Eq, Read, Show)
	, openFileEx	      	-- :: FilePath -> IOModeEx -> IO Handle

	, unsafePtrEq
	, unsafePtrToInt
	, unsafeCoerce
	
	  -- backward compatibility with IOExtensions
	, readBinaryFile        -- :: FilePath -> IO String
	, writeBinaryFile       -- :: FilePath -> String -> IO ()
	, appendBinaryFile      -- :: FilePath -> String -> IO ()
	, openBinaryFile        -- :: FilePath -> IOMode -> IO Handle

	, hSetBinaryMode	-- :: Handle -> Bool -> IO ()
	, hPutBuf	        -- :: Handle -> Ptr a -> Int -> IO ()
	, hGetBuf	        -- :: Handle -> Ptr a -> Int -> IO Int

	, argv                  -- :: [String]

	-- Terminal operations
	, hIsTerminalDevice	-- :: Handle -> IO Bool
	, hGetEcho		-- :: Handle -> IO Bool
	, hSetEcho		-- :: Handle -> Bool -> IO ()
	) where

import Hugs.Prelude
import Hugs.IO
import Hugs.System ( getArgs )
import Hugs.Ptr ( Ptr )

-----------------------------------------------------------------------------

primitive performGC "primGC" :: IO ()

unsafePerformIO :: IO a -> a
unsafePerformIO m = valueOf (basicIORun m)

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = IO (\ s -> s (unsafePerformIO m))

primitive unsafePtrEq    :: a -> a -> Bool
primitive unsafePtrToInt :: a -> Int

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b

valueOf :: IOFinished a -> a
valueOf (Finished_Return a) = a
valueOf _ = error "IOExts.valueOf: thread failed"	-- shouldn't happen

-----------------------------------------------------------------------------
-- Binary files 
-----------------------------------------------------------------------------
data IOModeEx 
 = BinaryMode IOMode
 | TextMode   IOMode
   deriving (Eq, Read, Show)

openFileEx :: FilePath -> IOModeEx -> IO Handle
openFileEx fp m = 
  case m of
    BinaryMode m -> openBinaryFile fp m
    TextMode m   -> openFile fp m

argv :: [String]
argv = unsafePerformIO getArgs

writeBinaryFile		:: FilePath -> String -> IO ()
writeBinaryFile		 = writeBinaryFile' WriteMode

appendBinaryFile	:: FilePath -> String -> IO ()
appendBinaryFile	 = writeBinaryFile' AppendMode

writeBinaryFile'	:: IOMode -> FilePath -> String -> IO ()
writeBinaryFile' mode name s = do
  h <- openBinaryFile name mode
  catchException (hPutStr h s) (\e -> hClose h >> throw e)
  hClose h

readBinaryFile		:: FilePath -> IO String
readBinaryFile name	 = openBinaryFile name ReadMode >>= hGetContents

primitive openBinaryFile         :: FilePath -> IOMode -> IO Handle

primitive hSetBinaryMode	 :: Handle -> Bool -> IO ()
primitive hPutBuf	    	 :: Handle -> Ptr a -> Int -> IO ()
primitive hGetBuf	    	 :: Handle -> Ptr a -> Int -> IO Int

primitive hIsTerminalDevice	 :: Handle -> IO Bool
primitive hGetEcho		 :: Handle -> IO Bool
primitive hSetEcho		 :: Handle -> Bool -> IO ()
