-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Utils
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Utility functions for a simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL.Utils
        (
	-- * Windows
	  openWindow		-- :: Title -> Size -> IO Window
	, clearWindow		-- :: Window -> IO ()
	, drawInWindow		-- :: Window -> Graphic -> IO ()
	, withWindow		-- :: Title -> Size -> (Window -> IO a) -> IO a
	, withWindow_		-- :: Title -> Size -> (Window -> IO a) -> IO ()
	, runWindow		-- :: Title -> Size -> (Window -> IO a) -> IO ()
	, getWindowSize		-- :: Window -> IO Size
	-- * Specific events
	-- ** Mouse events
	, getLBP		-- :: Window -> IO Point
	, getRBP		-- :: Window -> IO Point
	, getButton		-- :: Window -> Bool -> Bool -> IO Point
	-- ** Keyboard events
	, getKey		-- :: Window -> IO Key
	, getKeyEx		-- :: Window -> Bool -> IO Key
	, wGetChar		-- :: Window -> IO Char
	-- * Graphics
	-- ** Combining Graphics
	, emptyGraphic		-- :: Graphic
	, overGraphic		-- :: Graphic -> Graphic -> Graphic
	, overGraphics		-- :: [Graphic] -> Graphic
	-- ** Graphic modifiers
	, withFont		-- :: Font -> Graphic -> Graphic
	, withTextColor		-- :: RGB -> Graphic -> Graphic
	, withTextAlignment	-- :: Alignment -> Graphic -> Graphic
	, withBkColor		-- :: RGB -> Graphic -> Graphic
	, withBkMode		-- :: BkMode -> Graphic -> Graphic
	, withPen		-- :: Pen -> Graphic -> Graphic
	, withBrush		-- :: Brush -> Graphic -> Graphic
	, withRGB		-- :: RGB -> Graphic -> Graphic
	-- * Named colors
	, Color(..)
	, colorList		-- :: [(Color, RGB)]
	, colorTable		-- :: Array Color RGB
	, withColor		-- :: Color -> Graphic -> Graphic
	-- * Concurrency
	, par			-- :: IO a -> IO b -> IO (a, b)
	, par_			-- :: IO a -> IO b -> IO ()
	, parMany		-- :: [IO ()] -> IO ()

        ) where

import Graphics.HGL.Core
import Control.Concurrent
        ( newEmptyMVar, takeMVar, putMVar
        , forkIO
        )
import qualified Control.Exception as E
import Data.Ix(Ix)
import Data.Array(Array,array,(!))

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- | Create a window with the given title and size.
openWindow        :: Title -> Size -> IO Window
-- | Erase all drawing in the window.
-- (That is, set the 'Graphic' held by the window to 'emptyGraphic'.)
clearWindow       :: Window -> IO ()
-- | Draw the given graphic on the window, on top of anything that is
-- already there.
-- (That is, combine the given 'Graphic' and the one held by the window
-- using 'overGraphic', store the result in the window, and display it.)
drawInWindow      :: Window -> Graphic -> IO ()
-- | Run an action inside a new window, ensuring that the window is destroyed
-- on exit.
withWindow        :: Title -> Size -> (Window -> IO a) -> IO a
-- | A variant of 'withWindow' that ignores the result of the action.
withWindow_       :: Title -> Size -> (Window -> IO a) -> IO ()
-- | A combination of 'runGraphics' and 'withWindow_'.
runWindow         :: Title -> Size -> (Window -> IO a) -> IO ()

-- | The current size of the window.
getWindowSize     :: Window -> IO Size
-- | Wait for a press of the left mouse button,
-- and return the position of the mouse cursor.
getLBP            :: Window -> IO Point
-- | Wait for a press of the right mouse button,
-- and return the position of the mouse cursor.
getRBP            :: Window -> IO Point
-- | Wait for a mouse button to be pressed or released,
-- and return the position of the mouse cursor.
getButton         :: Window
                  -> Bool	-- ^ if 'True', wait for the left button
                  -> Bool	-- ^ if 'True', wait for a press;
				-- otherwise wait for a release.
                  -> IO Point
-- | Wait until a key is pressed and released.
getKey            :: Window -> IO Key
-- | Wait until a key is pressed (if the second argument is 'True')
-- or released (otherwise).
getKeyEx          :: Window -> Bool -> IO Key
-- | Wait for a translated character (from a key press).
-- Use in preference to 'getKey' if the aim is to read text.
wGetChar          :: Window -> IO Char

-- | An empty drawing.
emptyGraphic      :: Graphic
-- | A composite drawing made by overlaying the first argument on the second.
overGraphic       :: Graphic -> Graphic -> Graphic
-- | Overlay a list of drawings.
overGraphics      :: [Graphic] -> Graphic

-- | Set the default font for a drawing.
withFont          :: Font      -> Graphic -> Graphic
-- | Set the default color for drawing text.
withTextColor     :: RGB       -> Graphic -> Graphic
-- | Set the default alignment of text in a drawing.
withTextAlignment :: Alignment -> Graphic -> Graphic
-- | Set the default background color for drawing text with background
-- mode 'Opaque'.  The background color is ignored when the mode is
-- 'Transparent'.
withBkColor       :: RGB       -> Graphic -> Graphic
-- | Set the default background mode for drawing text.
withBkMode        :: BkMode    -> Graphic -> Graphic
-- | Set the default pen for drawing lines.
withPen           :: Pen       -> Graphic -> Graphic
-- | Set the default brush for filling shapes.
withBrush         :: Brush     -> Graphic -> Graphic
-- | A convenience function that sets the brush,
-- pen and text colors to the same value.
withRGB           :: RGB       -> Graphic -> Graphic

-- | Named colors.
data Color 
  = Black
  | Blue
  | Green 
  | Cyan
  | Red 
  | Magenta
  | Yellow
  | White
 deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

-- | A mapping of 'Color' names to 'RGB' triples.
colorList  :: [(Color, RGB)]
-- | A mapping of 'Color' names to 'RGB' triples.
colorTable :: Array Color RGB
-- | Set the default drawing color for a 'Graphic'.
withColor  :: Color -> Graphic -> Graphic

-- | Run two 'IO' actions in parallel and terminate when both actions terminate.
par               :: IO a -> IO b -> IO (a,b)
-- | Run two 'IO' actions in parallel and terminate when both actions terminate,
-- discarding the results of the actions.
par_              :: IO a -> IO b -> IO ()
-- | Run several 'IO' actions in parallel and terminate when all actions
-- terminate, discarding the results of the actions.
parMany           :: [IO ()] -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

-- Window operations

openWindow name size = openWindowEx name Nothing size Unbuffered Nothing

clearWindow w = setGraphic w emptyGraphic

getWindowSize w = do
        (pt,sz) <- getWindowRect w
        return sz

drawInWindow w p = do
        modGraphic w (p `overGraphic`)
        directDraw w p

withWindow name size = E.bracket (openWindow name size) closeWindow
withWindow_ name size f = withWindow name size f >> return ()
runWindow name size f = runGraphics (withWindow_ name size f)

-- Event operations

-- wait for left/right mouse button up (SOE p148)
getLBP w = getButton w True  True
getRBP w = getButton w False True

-- Wait for a key to go down then a (possibly different) key to go up
getKey w = do { getKeyEx w True; getKeyEx w False }

-- wait for key to go down/up
getKeyEx w down = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Key { keysym = k, isDown = isDown } 
            |  isDown == down 
            -> return k
          _ -> loop

getButton w left down = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Button {pt=pt,isLeft=isLeft,isDown=isDown} 
            | isLeft == left && isDown == down
            -> return pt
          _ -> loop

wGetChar w = loop
 where
  loop = do
        e <- getWindowEvent w
        case e of 
          Char {char = c} -> return c
          _               -> loop

-- Graphic

--elsewhere: type Graphic = Draw ()
emptyGraphic        = return ()
g1 `overGraphic` g2 = g2 >> g1
overGraphics        = foldr overGraphic emptyGraphic

-- Graphic modifiers

withFont          x = bracket_ (selectFont       x) selectFont
withTextAlignment x = bracket_ (setTextAlignment x) setTextAlignment
withTextColor     x = bracket_ (setTextColor     x) setTextColor
withBkColor       x = bracket_ (setBkColor       x) setBkColor
withBkMode        x = bracket_ (setBkMode        x) setBkMode
withPen           x = bracket_ (selectPen        x) selectPen
withBrush         x = bracket_ (selectBrush      x) selectBrush

withRGB c p = 
  mkBrush c       $ \ brush ->
  withBrush brush $
  mkPen Solid 2 c $ \ pen ->
  withPen pen     $
  withTextColor c $
  p

colorList =
  [ (Black   , RGB   0   0   0)
  , (Blue    , RGB   0   0 255)
  , (Green   , RGB   0 255   0)
  , (Cyan    , RGB   0 255 255)
  , (Red     , RGB 255   0   0)
  , (Magenta , RGB 255   0 255)
  , (Yellow  , RGB 255 255   0)
  , (White   , RGB 255 255 255)
  ]

colorTable = array (minBound, maxBound) colorList

withColor c g = withRGB (colorTable ! c) g 

-- Concurrency primitives

par m1 m2 = do
  v1 <- newEmptyMVar 
  v2 <- newEmptyMVar 
  forkIO (m1 >>= putMVar v1)
  forkIO (m2 >>= putMVar v2)
  a <- takeMVar v1
  b <- takeMVar v2
  return (a,b)

par_ m1 m2 = do
  v1 <- newEmptyMVar 
  v2 <- newEmptyMVar 
  forkIO (m1 >> putMVar v1 ())
  forkIO (m2 >> putMVar v2 ())
  takeMVar v1
  takeMVar v2
  return ()

parMany ms = foldr par_ (return ()) ms

----------------------------------------------------------------
-- End
----------------------------------------------------------------
