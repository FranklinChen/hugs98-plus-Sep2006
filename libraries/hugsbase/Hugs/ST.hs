-----------------------------------------------------------------------------
-- Strict State Thread module
-- 
-- This library provides support for strict state threads, as described
-- in the PLDI '94 paper by John Launchbury and Simon Peyton Jones.
-- In addition to the monad ST, it also provides mutable variables STRef
-- and mutable arrays STArray.
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Hugs.ST 
	( ST(..)
	, runST
	, unsafeRunST
	, RealWorld
	, stToIO
	, unsafeIOToST
	, unsafeSTToIO

	, STRef
	  -- instance Eq (STRef s a)
	, newSTRef
	, readSTRef
	, writeSTRef 

        , STArray
          -- instance Eq (STArray s ix elt)
        , newSTArray
        , boundsSTArray
        , readSTArray
        , writeSTArray
        , thawSTArray
        , freezeSTArray
        , unsafeFreezeSTArray

	, unsafeReadSTArray
	, unsafeWriteSTArray
	) where

import Hugs.Prelude(IO(..))
import Hugs.Array(Array,Ix(index,rangeSize),bounds,elems)
import Hugs.IOExts(unsafePerformIO, unsafeCoerce)
import Control.Monad   

-----------------------------------------------------------------------------

-- The ST representation generalizes that of IO (cf. Hugs.Prelude),
-- so it can use IO primitives that manipulate local state.

newtype ST s a = ST (forall r. (a -> r) -> r)

data RealWorld = RealWorld

primitive thenStrictST "primbindIO" :: ST s a -> (a -> ST s b) -> ST s b
primitive returnST     "primretIO"  :: a -> ST s a

unST                :: ST s a -> (a -> r) -> r
unST (ST f)          = f

runST               :: (forall s. ST s a) -> a
runST m              = unST m id

unsafeRunST         :: ST s a -> a
unsafeRunST m        = unST m id

stToIO              :: ST RealWorld a -> IO a
stToIO (ST f)        = IO f

unsafeIOToST        :: IO a -> ST s a
unsafeIOToST         = unsafePerformIO . liftM returnST

unsafeSTToIO        :: ST s a -> IO a
unsafeSTToIO         = stToIO . unsafeCoerce

instance Functor (ST s) where
    fmap = liftM

instance Monad (ST s) where
    (>>=)  = thenStrictST
    return = returnST

-----------------------------------------------------------------------------

data STRef s a   -- implemented as an internal primitive

primitive newSTRef   "newRef"     :: a -> ST s (STRef s a)
primitive readSTRef  "getRef"     :: STRef s a -> ST s a
primitive writeSTRef "setRef"     :: STRef s a -> a -> ST s ()
primitive eqSTRef    "eqRef"      :: STRef s a -> STRef s a -> Bool

instance Eq (STRef s a) where (==) = eqSTRef

-----------------------------------------------------------------------------

data STArray s ix elt -- implemented as an internal primitive

newSTArray          :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)
readSTArray         :: Ix ix => STArray s ix elt -> ix -> ST s elt
writeSTArray        :: Ix ix => STArray s ix elt -> ix -> elt -> ST s ()
thawSTArray         :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray       :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

unsafeReadSTArray   :: Ix i => STArray s i e -> Int -> ST s e
unsafeReadSTArray    = primReadArr

unsafeWriteSTArray  :: Ix i => STArray s i e -> Int -> e -> ST s ()
unsafeWriteSTArray   = primWriteArr

newSTArray bs e      = primNewArr bs (rangeSize bs) e
boundsSTArray a      = primBounds a
readSTArray a i      = unsafeReadSTArray a (index (boundsSTArray a) i)
writeSTArray a i e   = unsafeWriteSTArray a (index (boundsSTArray a) i) e
thawSTArray arr      = do
		       stArr <- newSTArray (bounds arr) err
		       sequence_ (zipWith (unsafeWriteSTArray stArr)
						[0..] (elems arr))
		       return stArr
 where
  err = error "thawArray: element not overwritten" -- shouldnae happen
freezeSTArray a      = primFreeze a
unsafeFreezeSTArray  = freezeSTArray  -- not as fast as GHC

instance Eq (STArray s ix elt) where
  (==) = eqSTArray

primitive primNewArr   "IONewArr"
          :: (a,a) -> Int -> b -> ST s (STArray s a b)
primitive primReadArr  "IOReadArr"
          :: STArray s a b -> Int -> ST s b
primitive primWriteArr "IOWriteArr"
          :: STArray s a b -> Int -> b -> ST s ()
primitive primFreeze   "IOFreeze"
          :: STArray s a b -> ST s (Array a b)
primitive primBounds   "IOBounds"
          :: STArray s a b -> (a,a)
primitive eqSTArray    "IOArrEq"
          :: STArray s a b -> STArray s a b -> Bool

-----------------------------------------------------------------------------
