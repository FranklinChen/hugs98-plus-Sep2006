-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Draw.Picture
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Drawing various shapes.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

module Graphics.HGL.Draw.Picture
	( arc, ellipse, shearEllipse
	, line, polyline, polygon
	, polyBezier -- becomes error message and polyline in X11
	) where

#if !X_DISPLAY_MISSING
import Graphics.HGL.X11.Types
import qualified Graphics.X11.Xlib as X
import System.IO.Unsafe(unsafePerformIO)
import System.IO(stderr, hPutStrLn)
#else
import Graphics.HGL.Win32.Types
import qualified Graphics.Win32 as Win32
#endif

import Graphics.HGL.Draw.Monad(Graphic)
import Graphics.HGL.Internals.Draw(mkDraw)
import Graphics.HGL.Units

----------------------------------------------------------------
-- The Interface (SOE, p50)
----------------------------------------------------------------

-- | A filled arc from an ellipse.
arc
  :: Point	-- ^ a corner of the rectangle bounding the ellipse.
  -> Point	-- ^ the opposite corner of the rectangle bounding the ellipse.
  -> Angle	-- ^ the start angle of the arc, measured counter-clockwise
		-- from the horizontal.
  -> Angle	-- ^ the extent of the arc, measured counter-clockwise from
		-- the start angle.
  -> Graphic	-- ^ a filled shape

-- | A filled ellipse that fits inside a rectangle defined by two
-- 'Point's on the window.
ellipse
  :: Point	-- ^ a corner of the rectangle bounding the ellipse.
  -> Point	-- ^ the opposite corner of the rectangle bounding the ellipse.
  -> Graphic	-- ^ a filled shape

-- | A filled sheared ellipse that fits inside a parallelogram defined
-- by three 'Point's on the window.  This function is implemented using
-- polygons on both Win32 and X11.
shearEllipse
  :: Point	-- ^ a corner of the bounding parallelogram.
  -> Point	-- ^ another corner of the parallelogram, adjacent to the first.
  -> Point	-- ^ another corner of the parallelogram, adjacent to the first
		-- and thus opposite to the second.
  -> Graphic	-- ^ a filled shape

-- | A filled polygon defined by a list of 'Point's.
polygon      :: [Point]                          -> Graphic  -- filled

-- | A line between two 'Point's.
line         :: Point -> Point           	 -> Graphic  -- unfilled

-- | A series of lines through a list of 'Point's.
polyline     :: [Point]                          -> Graphic  -- unfilled

-- | A series of (unfilled) Bezier curves defined by a list of 3/n/+1
-- control 'Point's.  This function is not supported on X11 (it yields
-- an error message and a 'polyline').
polyBezier   :: [Point]                          -> Graphic  -- unfilled

----------------------------------------------------------------
-- The Implementation
----------------------------------------------------------------

#if !X_DISPLAY_MISSING

arc (x0,y0) (x1,y1) s e = mkDraw (\ dc -> X.fillArc (disp dc) (drawable dc) (paintGC dc) x' y' w' h' s' e')
 where
  (x,w) = minAndDelta x0 x1
  (y,h) = minAndDelta y0 y1
  x' = fromIntegral x
  y' = fromIntegral y
  w' = fromIntegral w
  h' = fromIntegral h
  s' = round (s * 64)
  e' = round (e * 64)

ellipse (x0,y0) (x1,y1) = mkDraw (\ dc -> X.fillArc (disp dc) (drawable dc) (brushGC dc) x' y' w' h' 0 threeSixty)
 where
  (x,w) = minAndDelta x0 x1
  (y,h) = minAndDelta y0 y1
  x' = fromIntegral x
  y' = fromIntegral y
  w' = fromIntegral w
  h' = fromIntegral h

-- X measures angles in 64ths of a degree
threeSixty :: Int
threeSixty = 360*64

shearEllipse p0 p1 p2 = mkDraw (\ dc -> X.fillPolygon (disp dc) (drawable dc) (brushGC dc) pts X.convex X.coordModeOrigin)
 where
  X.Point x0 y0 = fromPoint p0
  X.Point x1 y1 = fromPoint p1
  X.Point x2 y2 = fromPoint p2

  x = avg x1 x2 -- centre of parallelogram
  y = avg y1 y2
  
  dx1 = fromIntegral ((x1 - x0) `div` 2) -- distance to corners from centre
  dy1 = fromIntegral ((y1 - y0) `div` 2)
  dx2 = fromIntegral ((x2 - x0) `div` 2)
  dy2 = fromIntegral ((y2 - y0) `div` 2)

  pts = [ X.Point (x + round(c*dx1 + s*dx2)) (y + round(c*dy1 + s*dy2))
        | (c,s) <- cos'n'sins
        ]

cos'n'sins :: [(Double,Double)]
cos'n'sins = [ (cos a, sin a) | a <- angles ]

angles :: [Angle]
angles = take 40 [0, pi/20 .. ]

line p0 p1 = mkDraw (\ dc -> X.drawLine (disp dc) (drawable dc) (paintGC dc) x0 y0 x1 y1)
 where
  X.Point x0 y0 = fromPoint p0
  X.Point x1 y1 = fromPoint p1

polyline pts = mkDraw (\ dc -> X.drawLines (disp dc) (drawable dc) (paintGC dc) (map fromPoint pts) X.coordModeOrigin)
polygon  pts = mkDraw (\ dc -> X.fillPolygon (disp dc) (drawable dc) (brushGC dc) (map fromPoint pts) X.complex X.coordModeOrigin)
polyBezier = unsafePerformIO $ do
  hPutStrLn stderr "warning: polyBezier is unavailable in X11 -- using polyline instead"
  return polyline

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

-- delta is always +ve
minAndDelta :: Int -> Int -> (Int,Int)
minAndDelta a b 
  | a <= b    = (a, b-a)
  | otherwise = (b, a-b)

-- avg :: Int32 -> Int32 -> Int32
avg :: Integral a => a -> a -> a
avg a b = (a + b) `div` 2

#else /* X_DISPLAY_MISSING */

arc p0 p1 start end = mkDraw (\ hdc -> Win32.arc hdc x0 y0 x1 y1 xs ys xe ye)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1
  x = (x0 + x1) `div` 2
  y = (y0 + y1) `div` 2
  start' = 2 * pi * start / 360
  end'   = 2 * pi * end   / 360
  xs = x + round (100 * cos start')
  ys = y + round (100 * sin start')
  xe = x + round (100 * cos end')
  ye = y + round (100 * sin end')

ellipse p0 p1  = mkDraw (\ hdc -> Win32.ellipse hdc x0 y0 x1 y1)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

shearEllipse p0 p1 p2 = mkDraw (\ hdc -> 
  Win32.transformedEllipse hdc (fromPoint p0) (fromPoint p1) (fromPoint p2))

line p0 p1 = mkDraw (\ hdc -> Win32.moveToEx hdc x0 y0 >> Win32.lineTo   hdc x1 y1)
 where 
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

polyline pts   = mkDraw (\ hdc -> Win32.polyline   hdc (map fromPoint pts))
polygon pts    = mkDraw (\ hdc -> Win32.polygon    hdc (map fromPoint pts))
polyBezier pts = mkDraw (\ hdc -> Win32.polyBezier hdc (map fromPoint pts))

#endif /* X_DISPLAY_MISSING */

----------------------------------------------------------------
-- End
----------------------------------------------------------------
