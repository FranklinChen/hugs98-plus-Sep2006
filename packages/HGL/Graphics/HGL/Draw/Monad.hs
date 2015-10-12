-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Draw.Monad
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- The 'Draw' monad, with graphical objects as a special case.
--
-----------------------------------------------------------------------------

module Graphics.HGL.Draw.Monad
	( Graphic	-- = Draw ()
	, Draw
	, ioToDraw	-- :: IO a -> Draw a

	, bracket	-- :: Draw a -> (a -> Draw b) -> (a -> Draw c) -> Draw c
	, bracket_	-- :: Draw a -> (a -> Draw b) -> Draw c -> Draw c
	) where

import Graphics.HGL.Internals.Draw
