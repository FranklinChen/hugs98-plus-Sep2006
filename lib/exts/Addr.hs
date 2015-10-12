-----------------------------------------------------------------------------
-- Machine Addresses:
-- Suitable for use with Hugs 98 on 32 bit machines.
-----------------------------------------------------------------------------
module Addr
	( Addr
	, nullAddr -- :: Addr
 	, plusAddr -- :: Addr -> Int -> Addr
	-- instance Eq   Addr
	-- instance Show Addr
	, ptrToAddr        -- :: Ptr a -> Addr
	, addrToPtr        -- :: Addr -> Ptr a
	, funPtrToAddr     -- :: FunPtr a -> Addr
	, addrToFunPtr     -- :: Addr -> FunPtr a
	) where

import Prelude
-- data Addr -- in Prelude

instance Eq   Addr where (==)      = primEqAddr
instance Show Addr where showsPrec = primShowsAddr

primitive nullAddr      "nullPtr"         :: Addr
primitive plusAddr      "plusPtr"         :: Addr -> Int -> Addr
primitive primShowsAddr "primShowsPtr"    :: Int -> Addr -> ShowS
primitive primEqAddr    "primEqPtr"       :: Addr -> Addr -> Bool

primitive ptrToAddr    "primUnsafeCoerce" :: Ptr a -> Addr
primitive addrToPtr    "primUnsafeCoerce" :: Addr -> Ptr a
primitive funPtrToAddr "primUnsafeCoerce" :: FunPtr a -> Addr
primitive addrToFunPtr "primUnsafeCoerce" :: Addr -> FunPtr a


-----------------------------------------------------------------------------
