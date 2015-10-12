-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Internals.Types
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  portable
--
-- Basic types for a simple graphics library.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

-- #hide
module Graphics.HGL.Internals.Types where

#if !X_DISPLAY_MISSING
import qualified Graphics.X11.Xlib as X
#else
import Graphics.Win32.Misc(timeGetTime)
import Control.Monad( liftM )
#endif

import Data.Ix(Ix)
import Data.Word(Word8)

----------------------------------------------------------------
-- Units
----------------------------------------------------------------

-- | A distance on the screen, measured in pixels.
type Dimension = Int
-- | A position within a window, measured in pixels to the right and down
-- from the top left corner.
type Point     = (Int,Int)
-- | A (width, height) pair, both measured in pixels.
type Size      = (Int,Int)
-- | An angle in degrees (0 to 360).
type Angle     = Double

-- | Time, measured in milliseconds.
type Time = Integer

-- | Time in milliseconds since some arbitrary epoch.
getTime :: IO Integer
#if !X_DISPLAY_MISSING
getTime = X.gettimeofday_in_milliseconds
#else
getTime = liftM toInteger timeGetTime
#endif

---------------------------------------------------------------
-- Drawing
----------------------------------------------------------------

-- | A color, comprising red, green and blue components.
data RGB = RGB Word8 Word8 Word8

-- | The style of line drawn by a pen.
data Style
  = Solid 
  | Dash	-- "-------"
  | Dot		-- "......."
  | DashDot	-- "_._._._"
  | DashDotDot	-- "_.._.._"
  | Null
  | InsideFrame

-- | Background mode for drawing text.
data BkMode
  = Opaque	-- ^ Draw text on a bounding rectangle filled with the
		-- current background color.
  | Transparent -- ^ Draw text without a background rectangle.

-- | How strings drawn with 'Graphics.HGL.Draw.Text.text' are positioned
-- relative to the specified reference point.
type Alignment = (HAlign, VAlign)

-- | Horizontal alignment of text.
-- Names have a tick to distinguish them from "Prelude" names.
data HAlign
 = Left'	-- ^ align the left edge of the text with the reference point
 | Center	-- ^ center the text with the reference point
 | Right'	-- ^ align the right edge of the text with the reference point
 deriving (Enum, Eq, Ord, Ix, Show)

-- | Vertical alignment of text.
data VAlign
 = Top		-- ^ align the top edge of the text with the reference point
 | Baseline	-- ^ align the baseline of the text with the reference point
 | Bottom	-- ^ align the bottom edge of the text with the reference point
 deriving (Enum, Eq, Ord, Ix, Show)

---------------------------------------------------------------
-- Windows
----------------------------------------------------------------

-- | Title of a window.
type Title = String

-- | How to draw in a window.
data RedrawMode
  = DoubleBuffered	-- ^ use a /double buffer/ to reduce flicker.
			-- You should probably use this for animations.
  | Unbuffered		-- ^ draw directly to the window.
			-- This runs slightly faster but is more prone
			-- to flicker.
