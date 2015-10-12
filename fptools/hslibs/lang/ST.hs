module ST
  {-# DEPRECATED "This functionality is now available from Control.Monad.ST, Data.STRef, and Data.Array.ST" #-} 
        (
	module Control.Monad.ST, 
	module Data.STRef,
	STArray,
	newSTArray,
	readSTArray,
	writeSTArray,
	boundsSTArray,
	thawSTArray,
	freezeSTArray,
	unsafeFreezeSTArray,
	unsafeThawSTArray,
    ) where

import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import GHC.Arr
