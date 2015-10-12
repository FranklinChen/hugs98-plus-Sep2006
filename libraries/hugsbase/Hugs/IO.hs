-----------------------------------------------------------------------------
-- Standard Library: IO operations, beyond those included in the prelude
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Hugs.IO (
    Handle,          -- instances: Eq, Show.
    HandlePosn,      -- instances: Eq, Show.
    
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    
    stdin, stdout, stderr,  -- :: Handle
    openFile,		    -- :: FilePath -> IOMode -> IO Handle
    hClose, 		    -- :: Handle -> IO ()

    hFileSize,		    -- :: Handle -> IO Integer

    hIsEOF,                 -- :: Handle -> IO Bool
    isEOF,                  -- :: IO Bool

    hSetBuffering,          -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,          -- :: Handle -> IO BufferMode

    hFlush,                 -- :: Handle -> IO ()
    hGetPosn,		    -- :: Handle -> IO HandlePosn
    hSetPosn,               -- :: HandlePosn -> IO ()
    hSeek,                  -- :: Handle -> SeekMode -> Integer -> IO ()
    hTell,                  -- :: Handle -> IO Integer

    hLookAhead,             -- :: Handle -> IO Char

    hWaitForInput,          -- :: Handle -> Int -> IO Bool

    hGetChar,               -- :: Handle -> IO Char
    hGetLine,               -- :: Handle -> IO String
    hGetContents,           -- :: Handle -> IO String

    hPutChar,               -- :: Handle -> Char -> IO ()
    hPutStr,                -- :: Handle -> String -> IO ()

    hIsOpen,		    -- :: Handle -> IO Bool
    hIsClosed,		    -- :: Handle -> IO Bool
    hIsReadable,            -- :: Handle -> IO Bool
    hIsWritable,            -- :: Handle -> IO Bool
    hIsSeekable,            -- :: Handle -> IO Bool

    -- Non-standard extensions 
    handleToFd,             -- :: Handle -> IO Int
    openFd                  -- :: Int -> Bool -> IOMode -> Bool -> IO Handle
    ) where

import Hugs.Prelude	( Handle, IOMode(..), stdin, stdout, stderr )
import Hugs.Prelude	( openFile, hClose, hPutChar, hPutStr )
import Hugs.Prelude	( hGetContents, hGetChar, hGetLine )
import Hugs.Prelude	( Ix(..) )
import System.IO.Error

-- data Handle

data BufferMode  =  NoBuffering | LineBuffering 
                 |  BlockBuffering (Maybe Int)
                    deriving (Eq, Ord, Read, Show)
data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

primitive hFileSize   :: Handle -> IO Integer

primitive hIsEOF      :: Handle -> IO Bool

isEOF               :: IO Bool
isEOF                = hIsEOF stdin

hSetBuffering       :: Handle  -> BufferMode -> IO ()
hSetBuffering h bMode = 
  case bMode of
    NoBuffering   -> hSetBuff h 0 0
    LineBuffering -> hSetBuff h 1 0
    BlockBuffering (Just x) -> hSetBuff h 2 x
    BlockBuffering _        -> hSetBuff h 2 0

primitive hSetBuff  :: Handle -> Int -> Int -> IO ()

hGetBuffering       :: Handle  -> IO BufferMode
hGetBuffering h = do
  (k, sz) <- hGetBuff h
  case k of
    1 -> return NoBuffering
    2 -> return LineBuffering
    3 -> return (BlockBuffering (Just sz))
     -- fatal - never to happen.
    _ -> error "IO.hGetBuffering: unknown buffering mode"

primitive hGetBuff :: Handle -> IO (Int,Int)

primitive hFlush   :: Handle -> IO ()

data HandlePosn = HandlePosn Handle Int deriving Eq

instance Show HandlePosn where
   showsPrec p (HandlePosn h pos) = 
        showsPrec p h . showString " at position " . shows pos

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = do
  p <- hGetPosnPrim h
  return (HandlePosn h p)

hTell :: Handle -> IO Integer
hTell h = do
  p <- hGetPosnPrim h
  return (toInteger p)

primitive hGetPosnPrim :: Handle -> IO Int

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn h p) = hSetPosnPrim h p

primitive hSetPosnPrim  :: Handle -> Int -> IO () 

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek h sMode int 
 | int >  fromIntegral (maxBound :: Int) ||
   int <  fromIntegral (minBound :: Int) =
   ioError (userError ("IO.hSeek: seek offset out of supported range"))
 | otherwise = 
   hSeekPrim h (fromEnum sMode) ((fromIntegral int)::Int)
  
primitive hSeekPrim :: Handle -> Int -> Int -> IO () 

primitive hWaitForInput :: Handle -> Int -> IO Bool

primitive hLookAhead    :: Handle -> IO Char

primitive hIsOpen,    
   	  hIsClosed,  
   	  hIsReadable,
   	  hIsWritable,
	  hIsSeekable :: Handle -> IO Bool

-----------------------------------------------------------------------------

-- Extract the file descriptor from a Handle, closing the Handle
primitive handleToFd :: Handle -> IO Int

--
-- Creating a handle from a file descriptor/socket.
--
primitive openFd    :: Int    -- file descriptor
		    -> Bool   -- True => it's a socket.
		    -> IOMode -- what mode to open the handle in.
		    -> Bool   -- binary?
		    -> IO Handle
