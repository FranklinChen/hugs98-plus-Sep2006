-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.DC
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.X11.DC
	( drawUnbuffered, drawBuffered, erase
	) where

import Graphics.HGL.X11.Types

import qualified Graphics.X11.Xlib as X

import Data.IORef( IORef, readIORef, writeIORef )
import Control.Concurrent( readMVar )
import Graphics.HGL.Internals.Draw

----------------------------------------------------------------
-- Draw
----------------------------------------------------------------

drawUnbuffered :: DC -> Draw () -> IO ()
drawUnbuffered dc p = do
    unDraw erase dc
    unDraw p dc

drawBuffered :: DC -> Draw () -> X.GC -> Int -> IORef (Maybe X.Pixmap) -> IO ()
drawBuffered dc p gc depth ref_mbuffer = do
    (_,(width,height)) <- readMVar (ref_rect dc)
    -- Note: The buffer is deallocated whenever the window size changes!
    mbuffer <- readIORef ref_mbuffer
    buffer <- case mbuffer of
                  Nothing -> X.createPixmap (disp dc)
		                            (drawable dc)
					    width
					    height
					    depth
		  Just buffer -> return buffer
    X.fillRectangle (disp dc) buffer gc 0 0 width height
    unDraw p dc{drawable=buffer}
    X.copyArea (disp dc) buffer (drawable dc) (paintGC dc) 0 0 width height 0 0
    writeIORef ref_mbuffer (Just buffer)

erase	:: Draw ()
erase	= mkDraw (\ dc -> X.clearWindow (disp dc) (drawable dc))

----------------------------------------------------------------
-- End
----------------------------------------------------------------
