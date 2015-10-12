-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.PeekPoke
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module with peek- and poke-related utilities.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.PeekPoke (
   poke1, peek1, peek3, peek6
) where

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peekElemOff,pokeElemOff) )

--------------------------------------------------------------------------------
-- The implementation is little bit verbose/redundant, but seems to generate
-- better code than mapM/zipWithM_.

--------------------------------------------------------------------------------

{-# INLINE poke1 #-}
poke1 :: Storable a => Ptr a -> a -> IO ()
poke1 ptr x =
   pokeElemOff ptr 0 x

--------------------------------------------------------------------------------

{-# INLINE peek1 #-}
peek1 :: Storable a => (a -> b) -> Ptr a -> IO b
peek1 f ptr = do
   x <- peekElemOff ptr 0
   return $ f x

{-# INLINE peek3 #-}
peek3 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO b
peek3 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   return $ f x y z

{-# INLINE peek6 #-}
peek6 :: Storable a => (a -> a -> a -> b) -> Ptr a -> IO (b, b)
peek6 f ptr = do
   x <- peekElemOff ptr 0
   y <- peekElemOff ptr 1
   z <- peekElemOff ptr 2
   u <- peekElemOff ptr 3
   v <- peekElemOff ptr 4
   w <- peekElemOff ptr 5
   return $ (f x y z, f u v w)
