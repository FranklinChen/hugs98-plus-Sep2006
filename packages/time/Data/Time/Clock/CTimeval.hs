{-# OPTIONS -ffi -Wall -Werror -cpp #-}

-- #hide
module Data.Time.Clock.CTimeval where

#ifndef mingw32_HOST_OS
-- All Unix-specific, this

import Foreign
import Foreign.C

data CTimeval = MkCTimeval CLong CLong

instance Storable CTimeval where
	sizeOf _ = (sizeOf (undefined :: CLong)) * 2
	alignment _ = alignment (undefined :: CLong)
	peek p = do
		s   <- peekElemOff (castPtr p) 0
		mus <- peekElemOff (castPtr p) 1
		return (MkCTimeval s mus)
	poke p (MkCTimeval s mus) = do
		pokeElemOff (castPtr p) 0 s
		pokeElemOff (castPtr p) 1 mus

foreign import ccall unsafe "time.h gettimeofday" gettimeofday :: Ptr CTimeval -> Ptr () -> IO CInt

-- | Get the current POSIX time from the system clock.
getCTimeval :: IO CTimeval
getCTimeval = with (MkCTimeval 0 0) (\ptval -> do
	result <- gettimeofday ptval nullPtr
	if (result == 0)
	 then peek ptval
	 else fail ("error in gettimeofday: " ++ (show result))
	)

#endif
