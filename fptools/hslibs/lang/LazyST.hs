module LazyST
  {-# DEPRECATED "This module has moved to Control.Monad.ST.Lazy" #-} 
	      ( module Control.Monad.ST.Lazy
	      , module Data.STRef.Lazy
	      , module LazyST
	      , STArray
	      ) where

import Control.Monad.ST.Lazy
import Data.STRef.Lazy

import Data.Ix		( Ix )
import Data.Array	( Array )
import ST		( STArray )
import qualified ST

newSTArray 	    :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
readSTArray   	    :: Ix ix => STArray s ix elt -> ix -> ST s elt 
writeSTArray	    :: Ix ix => STArray s ix elt -> ix -> elt -> ST s () 
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)  
thawSTArray 	    :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray	    :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

newSTArray ixs init   	= strictToLazyST (ST.newSTArray ixs init)
readSTArray arr ix      = strictToLazyST (ST.readSTArray arr ix)
writeSTArray arr ix v   = strictToLazyST (ST.writeSTArray arr ix v)
boundsSTArray arr       = ST.boundsSTArray arr
thawSTArray arr	        = strictToLazyST (ST.thawSTArray arr)
freezeSTArray arr       = strictToLazyST (ST.freezeSTArray arr)
unsafeFreezeSTArray arr = strictToLazyST (ST.unsafeFreezeSTArray arr)
unsafeThawSTArray arr   = strictToLazyST (ST.unsafeThawSTArray arr)
