%
% (c) The AQUA Project, Glasgow University, 1994-1997
%
\section[ByteArray]{The @ByteArray@ interface}

Immutable, read-only chunks of bytes, the @ByteArray@ collects
together the definitions in @PrelArr@ & @PrelByteArr@ and exports them
as one.

\begin{code}
module ByteArray
  {-# DEPRECATED "This library will go away soon; use Data.Array.Unboxed instead" #-} 
       (
        -- This module is DEPRECATED.  Use IArray instead.

        ByteArray(..),  -- not abstract, for now. Instance of : CCallable, Eq.
        Ix,

	newByteArray,	      -- :: Ix ix => (ix,ix) -> ST s (ByteArray ix)

        --Indexing of ordinary @Arrays@ is standard Haskell and isn't defined here.
        indexCharArray,       -- :: Ix ix => ByteArray ix -> ix -> Char 
        indexIntArray,        -- :: Ix ix => ByteArray ix -> ix -> Int
        indexWordArray,       -- :: Ix ix => ByteArray ix -> ix -> Word
        indexAddrArray,       -- :: Ix ix => ByteArray ix -> ix -> Addr
        indexFloatArray,      -- :: Ix ix => ByteArray ix -> ix -> Float
        indexDoubleArray,     -- :: Ix ix => ByteArray ix -> ix -> Double
        indexStablePtrArray,  -- :: Ix ix => ByteArray ix -> ix -> (StablePtr a)

        sizeofByteArray,      -- :: Ix ix => ByteArray ix -> Int
        boundsOfByteArray     -- :: Ix ix => ByteArray ix -> (ix, ix)

       ) where

import PrelByteArr
import Addr

import GHC.Word
import GHC.Arr
import GHC.Base
import GHC.Stable( StablePtr(..) )
import GHC.ST
import Data.Ix
\end{code}

\begin{code}
indexStablePtrArray    :: Ix ix => ByteArray ix -> ix -> (StablePtr a)
indexStablePtrArray (ByteArray l u barr#) n
  = case (index (l,u) n)    	    	of { I# n# ->
    case indexStablePtrArray# barr# n# 	of { r# ->
    (StablePtr r#)}}
\end{code}

The size returned is in bytes.

\begin{code}
sizeofByteArray :: Ix ix => ByteArray ix -> Int
sizeofByteArray (ByteArray _ _ arr#) = 
  case (sizeofByteArray# arr#) of
    i# -> (I# i#)

boundsOfByteArray :: Ix ix => ByteArray ix -> (ix, ix)
boundsOfByteArray (ByteArray     l u _) = (l,u)
\end{code}

\begin{code}
newByteArray :: Ix ix => (ix,ix) -> ST s (ByteArray ix)
newByteArray ixs = do
   m_arr <- newCharArray ixs
   unsafeFreezeByteArray m_arr
\end{code}

If it should turn out to be an issue, could probably be speeded
up quite a bit.

\begin{code}
instance Ix ix => Eq (ByteArray ix) where
   b1 == b2 = eqByteArray b1 b2

eqByteArray :: Ix ix => ByteArray ix -> ByteArray ix -> Bool
eqByteArray b1 b2 =
  sizeofByteArray b1 == sizeofByteArray b2 &&
  all (\ x -> indexCharArray b1 x == indexCharArray b2 x) (range (boundsOfByteArray b1))
\end{code}

\begin{code}
indexAddrArray   :: Ix ix => ByteArray ix -> ix -> Addr
indexWordArray   :: Ix ix => ByteArray ix -> ix -> Word
{-# SPECIALIZE indexAddrArray   :: ByteArray Int -> Int -> Addr #-}
{-# SPECIALIZE indexWordArray   :: ByteArray Int -> Int -> Word #-}

indexAddrArray (ByteArray l u barr#) n
  = case (index (l,u) n)              of { I# n# ->
    case indexAddrArray# barr# n#     of { r# ->
    (A# r#)}}

indexWordArray (ByteArray l u barr#) n
  = case (index (l,u) n)	    	of { I# n# ->
    case indexWordArray# barr# n# 	of { r# ->
    (W# r#)}}
\end{code}
