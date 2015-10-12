%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[GlaExts]{The @GlaExts@ interface}

Compatibility cruft: Deprecated! Don't use!  This rug will
dissappear from underneath your feet very soon.

This module will eventually be the interface to GHC-ONLY extensions:
i.e. unboxery and primitive operations over unboxed values.

OLD:
The @GlaExts@ packages up various Glasgow extensions and
exports them all through one interface. The Idea being that
a Haskell program using a Glasgow extension doesn't have to
selective import of obscure/likely-to-move (believe me, we
really like to move functions around for the prelude bits!)
GHC interfaces - instead import the GlaExts rag bag and you should be away!

\begin{code}
module GlaExts
  {-# DEPRECATED "This library will go away soon; use GHC.Exts instead" #-} 
       (
	unsafePerformIO, 
	unsafeInterleaveIO,
        
        -- operations for interfacing IO and ST
        --
        stToIO,       -- :: ST RealWorld a -> IO a
	ioToST,	      -- :: IO a -> ST RealWorld a

        -- Everything from module ByteArray:
	module ByteArray,

        -- Same for Mutable(Byte)Array interface:
	module MutableArray,
	
        -- the representation of some basic types:
        Int(..),Addr(..),Word(..),Float(..),Double(..),Integer(..),Char(..),

	-- Fusion
	build, augment,

        -- misc bits
	trace,

	-- shifty wrappers from PrelBase
	shiftL#, shiftRL#, iShiftL#, iShiftRA#, iShiftRL#,

        -- and finally, all the unboxed primops of GHC.Prim!
        module GHC.Prim

       ) where

import GHC.Prim
import GHC.Base
import GHC.Word
import GHC.Num	( Integer(..) )
import GHC.Float( Float(..), Double(..) )
import IOExts
import GHC.IOBase
import ByteArray
import MutableArray hiding ( Ix )
import Addr
import Control.Monad
import Debug.Trace 	( trace )

type PrimIO a = IO a

primIOToIO :: PrimIO a -> IO a
primIOToIO io = io

ioToPrimIO :: IO a -> PrimIO a
ioToPrimIO io = io

unsafePerformPrimIO :: PrimIO a -> a
unsafePerformPrimIO = unsafePerformIO

thenPrimIO :: PrimIO a -> (a -> PrimIO b) -> PrimIO b
thenPrimIO = (>>=)

seqPrimIO :: PrimIO a -> PrimIO b -> PrimIO b
seqPrimIO = (>>)

returnPrimIO :: a -> PrimIO a
returnPrimIO = return

thenIO_Prim :: PrimIO a -> (a -> IO b) -> IO b
thenIO_Prim = (>>=)

-- ST compatibility stubs.
thenST :: ST s a -> ( a -> ST s b) -> ST s b
thenST = (>>=)

seqST :: ST s a -> ST s b -> ST s b
seqST = (>>)

returnST :: a -> ST s a
returnST = return

\end{code}
