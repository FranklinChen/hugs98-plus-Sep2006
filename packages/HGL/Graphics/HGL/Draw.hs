-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Draw
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Drawing in a simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL.Draw
	(
	-- * Graphics
	-- | The type 'Graphic', which represents an abstract drawing,
	-- is actually a special case of a 'Draw' monad.
	  module Graphics.HGL.Draw.Monad

	-- * Graphical objects
	-- | These are ways of constructing values of type 'Graphic'.
	, module Graphics.HGL.Draw.Picture
	, module Graphics.HGL.Draw.Text
	, module Graphics.HGL.Draw.Region

	-- * Graphical attributes
	-- | These are used to alter the above drawings.
	-- Brushes are used for filling shapes, pens for drawing lines.
	, module Graphics.HGL.Draw.Brush
	, module Graphics.HGL.Draw.Pen
	, module Graphics.HGL.Draw.Font
	) where

import Graphics.HGL.Draw.Monad
import Graphics.HGL.Draw.Picture
import Graphics.HGL.Draw.Text
import Graphics.HGL.Draw.Region
import Graphics.HGL.Draw.Brush
import Graphics.HGL.Draw.Pen
import Graphics.HGL.Draw.Font

----------------------------------------------------------------
-- The end
----------------------------------------------------------------
