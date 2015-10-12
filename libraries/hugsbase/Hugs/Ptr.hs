-----------------------------------------------------------------------------
-- Machine Addresses:
-- Suitable for use with Hugs 98 on 32 bit machines.
-----------------------------------------------------------------------------
module Hugs.Ptr
	( Ptr
	, nullPtr          -- :: Ptr a
 	, plusPtr          -- :: Ptr a -> Int -> Ptr b
        , castPtr          -- :: Ptr a -> Ptr b
	, alignPtr         -- :: Ptr a -> Int -> Ptr a
	, minusPtr         -- :: Ptr a -> Ptr b -> Int
	-- instance Eq   (Ptr a)
	-- instance Ord  (Ptr a)
	-- instance Show (Ptr a)

        , FunPtr
	, nullFunPtr        -- :: FunPtr a
	, castFunPtr        -- :: FunPtr a -> FunPtr b
	, castFunPtrToPtr   -- :: FunPtr a -> Ptr b
	, castPtrToFunPtr   -- :: Ptr a -> FunPtr b
        , freeHaskellFunPtr -- :: FunPtr a -> IO ()
	-- instance Eq   (FunPtr a)
	-- instance Ord  (FunPtr a)
	-- instance Show (FunPtr a)
	) where

import Hugs.Prelude ( Ptr, FunPtr )

-- data Ptr a -- in Hugs.Prelude

instance Eq   (Ptr a) where (==)      = primEqPtr
instance Ord  (Ptr a) where compare   = primCmpPtr
instance Show (Ptr a) where showsPrec = primShowsPtr

primitive nullPtr                    :: Ptr a
primitive plusPtr                    :: Ptr a -> Int -> Ptr b
primitive alignPtr                   :: Ptr a -> Int -> Ptr a
primitive minusPtr                   :: Ptr a -> Ptr b -> Int
primitive castPtr "primUnsafeCoerce" :: Ptr a -> Ptr b
primitive primShowsPtr               :: Int -> Ptr a -> ShowS
primitive primEqPtr                  :: Ptr a -> Ptr a -> Bool
primitive primCmpPtr                 :: Ptr a -> Ptr a -> Ordering

-- data FunPtr a -- in Hugs.Prelude

instance Eq   (FunPtr a) where (==)      = primEqFPtr
instance Ord  (FunPtr a) where compare   = primCmpFPtr
instance Show (FunPtr a) where showsPrec = primShowsFPtr

primitive nullFunPtr    "nullPtr"       :: FunPtr a
primitive primShowsFPtr "primShowsPtr"  :: Int -> FunPtr a -> ShowS
primitive primEqFPtr    "primEqPtr"     :: FunPtr a -> FunPtr a -> Bool
primitive primCmpFPtr   "primCmpPtr"    :: FunPtr a -> FunPtr a -> Ordering
primitive castFunPtr "primUnsafeCoerce" :: FunPtr a -> FunPtr b
primitive castFunPtrToPtr "primUnsafeCoerce" :: FunPtr a -> Ptr b
primitive castPtrToFunPtr "primUnsafeCoerce" :: Ptr a -> FunPtr b
primitive freeHaskellFunPtr             :: FunPtr a -> IO ()

-----------------------------------------------------------------------------
