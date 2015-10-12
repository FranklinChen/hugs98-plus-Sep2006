-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Window
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Windows in a simple graphics library.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

module Graphics.HGL.Window
	(
	-- * Windows
	  Window
	, Title			-- = String
	, RedrawMode(Unbuffered, DoubleBuffered)
	, openWindowEx		-- :: Title -> Maybe Point -> Maybe Size ->
				--    RedrawMode -> Maybe Time -> IO Window
	, getWindowRect		-- :: Window -> IO (Point,Point)
	, closeWindow		-- :: Window -> IO ()

	-- * Drawing in a window
	, setGraphic		-- :: Window -> Graphic -> IO ()
	, getGraphic		-- :: Window -> IO Graphic
	, modGraphic		-- :: Window -> (Graphic -> Graphic) -> IO ()
	, directDraw		-- :: Window -> Graphic -> IO ()
	-- not in X11: , redrawWindow		-- :: Window -> IO ()

	-- * Events in a window
	, Event(..)
	-- , Event(Char,Key,Button,MouseMove,Resize,Closed) -- deriving(Show)
	-- , char		-- :: Event -> Char
	-- , keysym		-- :: Event -> Key
	-- , isDown		-- :: Event -> Bool
	-- , pt			-- :: Event -> Point
	-- , isLeft		-- :: Event -> Bool
	, getWindowEvent	-- :: Window -> IO Event
	, maybeGetWindowEvent	-- :: Window -> IO (Maybe Event)

	-- * Timer ticks
	-- | Timers that tick at regular intervals are set up by 'openWindowEx'.
	, getWindowTick		-- :: Window -> IO ()
	, getTime		-- :: IO Time
	) where

#ifdef __HADDOCK__
import Graphics.HGL.Key
#endif
import Graphics.HGL.Units
import Graphics.HGL.Draw( Graphic )
import Graphics.HGL.Internals.Event( Event(..) )
import Graphics.HGL.Internals.Types( Title, RedrawMode(..), getTime )
import qualified Graphics.HGL.Internals.Events as E
import Graphics.HGL.Internals.Utilities( modMVar, modMVar_ )
#if !X_DISPLAY_MISSING
import Graphics.HGL.X11.Window (Window(..))
import qualified Graphics.HGL.X11.Window as X (openWindowEx, closeWindow,
	redrawWindow, directDraw, getWindowRect )
#else
import Graphics.HGL.Win32.WND
	(WND, openWND, getHWND, closeWND, wndRect, redrawWND, drawWND)
import Graphics.HGL.Win32.Types
import Graphics.HGL.Win32.Draw( 
	drawGraphic, drawBufferedGraphic 
	)
import Graphics.HGL.Draw	(Draw)
-- import Graphics.HGL.Internals.Types

import qualified Graphics.Win32 as Win32
#endif

import Control.Concurrent.MVar

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- | Wait for the next event on the given window.
getWindowEvent      :: Window -> IO Event

-- | Check for a pending event on the given window.
maybeGetWindowEvent :: Window -> IO (Maybe Event)

-- | Wait for the next tick event from the timer on the given window.
getWindowTick       :: Window -> IO ()

-- | Get the current drawing in a window.
getGraphic :: Window -> IO Graphic

-- | Set the current drawing in a window.
setGraphic :: Window -> Graphic -> IO ()

-- | Update the drawing for a window.
-- Note that this does not force a redraw.
modGraphic :: Window -> (Graphic -> Graphic) -> IO ()

-- | General window creation.
openWindowEx
  :: Title		-- ^ title of the window
  -> Maybe Point	-- ^ the optional initial position of a window
  -> Size		-- ^ initial size of the window
  -> RedrawMode		-- ^ how to display a graphic on the window
  -> Maybe Time		-- ^ the time between ticks (in milliseconds) of an
			-- optional timer associated with the window
  -> IO Window

-- | Close the window.
closeWindow   :: Window -> IO ()

redrawWindow  :: Window -> IO ()

directDraw    :: Window -> Graphic -> IO ()

-- | The position of the top left corner of the window on the screen,
-- and the size of the window.
getWindowRect :: Window -> IO (Point, Size)

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

getWindowEvent w     = E.getEvent (events w)

maybeGetWindowEvent w
  = do noEvent <- E.isNoEvent (events w)
       if noEvent 
          then return Nothing
          else do ev <- getWindowEvent w
                  return (Just ev)

getWindowTick w      = E.getTick (events w)

getGraphic w = readMVar (graphic w)

setGraphic w p = do
  modMVar (graphic w) (const p)
  redrawWindow w

modGraphic w = modMVar_ (graphic w)

#if !X_DISPLAY_MISSING

openWindowEx    = X.openWindowEx
closeWindow     = X.closeWindow
getWindowRect   = X.getWindowRect
redrawWindow    = X.redrawWindow
directDraw      = X.directDraw

#else /* X_DISPLAY_MISSING */

data Window = MkWindow { 
	events  :: E.Events,	    -- the event stream
	graphic :: MVar (Draw ()),  -- the current graphic
	wnd     :: WND	    	    -- the real window
	}

openWindowEx name pos size redrawMode tickRate = do
	graphic <- newMVar (return ())
	events  <- E.newEvents
	let draw = \ hwnd hdc -> do
                      p <- readMVar graphic 
		      repaint p hwnd hdc
	wnd     <- openWND name (fmap fromPoint pos) (Just $ fromPoint size) 
			   events draw (fmap fromInteger tickRate)
	mkWindow wnd events graphic
 where
  repaint = case redrawMode of
            Unbuffered     -> drawGraphic
            DoubleBuffered -> drawBufferedGraphic

mkWindow       :: WND -> E.Events -> MVar (Draw ()) -> IO Window
mkWindow wnd events graphic = do
	return (MkWindow { wnd=wnd, events=events, graphic=graphic })

closeWindow w   = closeWND  (wnd w)
getWindowRect w = wndRect   (wnd w)
redrawWindow w  = redrawWND (wnd w)
directDraw w p  = drawWND   (wnd w) p

-- in case you need low level access
windowHWND     :: Window -> IO Win32.HWND
windowHWND w    = getHWND   (wnd w)

#endif /* X_DISPLAY_MISSING */

----------------------------------------------------------------
-- End
----------------------------------------------------------------
