-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Internals.Draw
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- Drawing in a simple graphics library.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

-- #hide
module Graphics.HGL.Internals.Draw
	( Graphic	-- = Draw ()
	, Draw
	, ioToDraw	-- :: IO a -> Draw a

	, bracket	-- :: Draw a -> (a -> Draw b) -> (a -> Draw c) -> Draw c
	, bracket_	-- :: Draw a -> (a -> Draw b) -> Draw c -> Draw c

	, unDraw	-- :: Draw a -> (DC -> IO a)
	, mkDraw 	-- :: (DC -> IO a) -> Draw a
	) where

#if !X_DISPLAY_MISSING
import Graphics.HGL.X11.Types(DC)
#else
import Graphics.HGL.Win32.Types(DC)
#endif
import qualified Graphics.HGL.Internals.Utilities as Utils (bracket, bracket_)
import Control.Monad (liftM)

----------------------------------------------------------------
-- Graphics
----------------------------------------------------------------

-- | An abstract representation of an image.
type Graphic = Draw ()

-- | Monad for sequential construction of images.
newtype Draw a = MkDraw (DC -> IO a)

unDraw :: Draw a -> (DC -> IO a)
unDraw (MkDraw m) = m

-- | Embed an 'IO' action in a drawing action.
ioToDraw :: IO a -> Draw a
ioToDraw m = MkDraw (\ _ -> m)

mkDraw :: (DC -> IO a) -> Draw a
mkDraw = MkDraw

-- a standard reader monad
instance Monad Draw where
  return a = MkDraw (\ hdc -> return a)
  m >>= k  = MkDraw (\ hdc -> do { a <- unDraw m hdc; unDraw (k a) hdc })
  m >>  k  = MkDraw (\ dc -> do { unDraw m dc; unDraw k dc })

instance Functor Draw where fmap = liftM

-- | Wrap a drawing action in initialization and finalization actions.
bracket
  :: Draw a		-- ^ a pre-operation, whose value is passed to the
			-- other two components.
  -> (a -> Draw b)	-- ^ a post-operation, to be performed on exit from
			-- the bracket, whether normal or by an exception.
  -> (a -> Draw c)	-- ^ the drawing action inside the bracket.
  -> Draw c
bracket left right m = MkDraw (\ hdc ->
  Utils.bracket (unDraw left hdc) 
                (\ a -> unDraw (right a) hdc)
                (\ a -> unDraw (m a) hdc))

-- | A variant of 'bracket' in which the inner drawing action does not
-- use the result of the pre-operation.
bracket_
  :: Draw a		-- ^ a pre-operation, whose value is passed to the
			-- other two components.
  -> (a -> Draw b)	-- ^ a post-operation, to be performed on exit from
			-- the bracket, whether normal or by an exception.
  -> Draw c		-- ^ the drawing action inside the bracket.
  -> Draw c
bracket_ left right m = MkDraw (\ hdc ->
  Utils.bracket_ (unDraw left hdc) 
                 (\ a -> unDraw (right a) hdc)
                 (unDraw m hdc))

----------------------------------------------------------------
