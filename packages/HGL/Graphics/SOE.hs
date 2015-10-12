-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SOE
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (requires concurrency)
--
-- The graphics library used in /The Haskell School of Expression/,
-- by Paul Hudak, cf <http://www.haskell.org/soe/>.
--
-- /Notes:/
--
-- * This module is called @SOEGraphics@ in the book.  It is a cut
--   down version of "Graphics.HGL", with the interface frozen to match
--   the book.
--
-- * In chapters 13, 17 and 19 of the book, there are imports of modules
--   @Win32Misc@ and @Word@.  These should be omitted, as 'timeGetTime'
--   and 'word32ToInt' are provided by this module.
-----------------------------------------------------------------------------

module Graphics.SOE
	(
	-- * Getting started
	  runGraphics		-- p41

	-- * Windows
	, Title			-- p40
	, Size
	, Window
	, openWindow
	, getWindowSize		-- not in SOE, but Resize is
	, clearWindow		-- used on p127
	, drawInWindow		-- p41
	, drawInWindowNow	-- backward compatibility (p281)
	, setGraphic		-- p168
	, closeWindow		-- p41

	-- ** General windows
	, openWindowEx		-- p168

	, RedrawMode		-- SOE has (Graphic -> DrawFun)
	, drawGraphic		-- p168
	, drawBufferedGraphic

	-- * Drawing
	, Graphic		-- p41
	, emptyGraphic		-- p171
	, overGraphic
	, overGraphics		-- not in SOE, but an obvious extension

	-- ** Color
	, Color(..)		-- p43
	, withColor

	-- ** Drawing text
	, text			-- p41

	-- ** Drawing shapes
	, Point
	, ellipse		-- p43
	, shearEllipse
	, line
	, polygon
	, polyline
	, polyBezier -- warning: becomes error message and polyline in X11

	, Angle			-- not in SOE
	, arc			-- not in SOE, but handy for pie charts

	-- ** Regions
	, Region		-- p117
	, createRectangle
	, createEllipse
	, createPolygon
	, andRegion
	, orRegion
	, xorRegion
	, diffRegion
	, drawRegion

	-- * User interaction

	-- ** Keyboard events
	, getKey		-- p41

	-- ** Mouse events
	, getLBP		-- used on p127
	, getRBP		-- not in SOE, but obvious

	-- ** General events
	, Event(..)		-- p214
	, maybeGetWindowEvent	-- p248
	, getWindowEvent	-- not in SOE, but obvious

	-- * Time
	-- Timers that tick at regular intervals are set up by 'openWindowEx'.
	, Word32		-- p168
	, getWindowTick

	, timeGetTime		-- from Win32
	, word32ToInt		-- obsolete function from Data.Word

	) where

import Graphics.HGL
	hiding (getKey, getKeyEx, openWindowEx,
		Event(..), getWindowEvent, maybeGetWindowEvent)
import qualified Graphics.HGL as HGL
import Control.Monad(liftM)
import Data.Word(Word32)

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- | A rectangular region, with the given points as opposite corners.
createRectangle     :: Point -> Point -> Region

-- | A polygonal region defined by a list of 'Point's.
createPolygon       :: [Point] -> Region

-- | An elliptical region that fits in the rectangle with the given points
-- as opposite corners.
createEllipse       :: Point -> Point -> Region

-- | The union of two regions.
orRegion            :: Region -> Region -> Region

-- | The intersection of two regions.
andRegion           :: Region -> Region -> Region

-- | The part of the first region that is not also in the second.
diffRegion          :: Region -> Region -> Region

-- | Draw a 'Region' in the current color.
drawRegion          :: Region -> Graphic

-- | Another name for 'drawInWindow', retained for backwards compatibility.
drawInWindowNow     :: Window -> Graphic -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- | an extended version of 'openWindow'.
openWindowEx :: Title		-- ^ the title of the window
             -> Maybe Point	-- ^ the initial position of the window
             -> Maybe Size	-- ^ the initial size of the window
             -> RedrawMode	-- ^ how to display a graphic on the window
             -> Maybe Word32	-- ^ optionally attach a timer to the window,
				-- with the specified time (in milliseconds)
				-- between ticks.
             -> IO Window
openWindowEx a b (Just c) d e =
  HGL.openWindowEx a b c d (fmap fromIntegral e)
openWindowEx a b Nothing d e =
  HGL.openWindowEx a b (300,300) d (fmap fromIntegral e)

createRectangle = rectangleRegion
createEllipse   = ellipseRegion
createPolygon   = polygonRegion
orRegion        = unionRegion
andRegion       = intersectRegion
diffRegion      = subtractRegion
drawRegion      = regionToGraphic

-- backwards compatibility:

-- | Draw directly to the window
-- (slightly faster than 'drawBufferedGraphic', but more prone to flicker).
drawGraphic         :: RedrawMode
drawGraphic          = Unbuffered

-- | Use a /double buffer/ to reduce flicker and thus improve the look
-- of animations.
drawBufferedGraphic :: RedrawMode
drawBufferedGraphic  = DoubleBuffered

-- should have a different way to specify background color
-- drawBufferedGraphicBC :: RGB -> RedrawMode

drawInWindowNow = drawInWindow

-- | The current time of day (in milliseconds).
timeGetTime         :: IO Word32
timeGetTime = liftM integerToWord32 getTime

integerToWord32 :: Integer -> Word32
#ifdef __GLASGOW_HASKELL__
integerToWord32 = fromInteger	-- conversion to Word32 doesn't overflow
#else
integerToWord32 n = fromInteger (n `mod` (toInteger (maxBound::Word32) + 1))
#endif

-- | An obsolete special case of 'fromIntegral'.
word32ToInt         :: Word32 -> Int
word32ToInt = fromIntegral

----------------------------------------------------------------
-- Event, getKey, and maybeGetWindowEvent compatibility
----------------------------------------------------------------

{-
 The SOE sources are set in stone, so this module provides the interface
 SOE expects, even if the Graphics library moves on (cf. Event.Key).
-}

-- Deprecated SOE compatibility.

-- | Wait until a key is pressed and released,
-- and return the corresponding character.
getKey :: Window -> IO Char
getKey w = do { getKeyEx w True; getKeyEx w False }

-- | Wait until a key is pressed (if the second argument is 'True')
-- or released (otherwise), and return the corresponding character.
-- (not in SOE)
getKeyEx :: Window -> Bool -> IO Char
getKeyEx w down = loop
 where
  loop = do
        e <- HGL.getWindowEvent w
        case e of
          HGL.Key { HGL.keysym = k, HGL.isDown = isDown }
            |  isDown == down && isCharKey k
            -> return (keyToChar k)
          _ -> loop

-- | Wait for the next event in the window.
getWindowEvent :: Window -> IO Event
getWindowEvent w = liftM toSOEEvent (HGL.getWindowEvent w)

-- | Return a pending eventin the window, if any.
maybeGetWindowEvent :: Window -> IO (Maybe Event)
maybeGetWindowEvent w = liftM (fmap toSOEEvent) (HGL.maybeGetWindowEvent w)

-- tiresome, but necessary.
toSOEEvent :: HGL.Event -> Event
toSOEEvent (HGL.Char x)       = Key x True
toSOEEvent (HGL.Key k isDown) = Key (keyToChar k) isDown
toSOEEvent (HGL.Button pt left down) = Button pt left down
toSOEEvent (HGL.MouseMove p)  = MouseMove p
toSOEEvent (HGL.Resize)       = Resize
toSOEEvent (HGL.Closed)       = Closed

-- | User interface events
data Event
  = Key
    { char :: Char	-- ^ character corresponding to the key
    , isDown :: Bool	-- ^ if 'True', the key was pressed;
			-- otherwise it was released
    }			-- ^ occurs when a key was pressed or released.
  | Button
    { pt :: Point	-- ^ the position of the mouse cursor
    , isLeft :: Bool	-- ^ if 'True', it was the left button
    , isDown :: Bool	-- ^ if 'True', the button was pressed;
			-- otherwise it was released
    }			-- ^ occurs when a mouse button is pressed or released.
  | MouseMove
    { pt :: Point	-- ^ the position of the mouse cursor
    }			-- ^ occurs when the mouse is moved inside the window.
  | Resize		-- ^ occurs when the window is resized.
			-- The new window size can be discovered using
			-- 'getWindowSize'.
  | Closed		-- ^ occurs when the window is closed.
 deriving Show

----------------------------------------------------------------
-- End
----------------------------------------------------------------
