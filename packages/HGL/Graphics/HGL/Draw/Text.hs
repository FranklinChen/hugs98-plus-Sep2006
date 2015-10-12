-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Draw.Text
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Drawing text.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

module Graphics.HGL.Draw.Text
        (
	-- * Drawing text
          text
	-- ToDo: add textInfo to Win32
#if !X_DISPLAY_MISSING
        , textInfo
#endif
	-- * Color
	, RGB(RGB)
	, setTextColor		-- :: RGB -> Draw RGB
	, setBkColor		-- :: RGB -> Draw RGB
	, BkMode(Opaque, Transparent)
	, setBkMode		-- :: BkMode -> Draw BkMode
	-- * Alignment
	, Alignment		-- = (HAlign, VAlign)
	, HAlign(Left', Center, Right')
	, VAlign(Top, Baseline, Bottom)
	, setTextAlignment	-- :: Alignment -> Draw Alignment
	) where

#if !X_DISPLAY_MISSING
import qualified Graphics.X11.Xlib as X
import Graphics.HGL.X11.Types
import Control.Concurrent.MVar (readMVar, takeMVar, putMVar)
#else
import qualified Graphics.Win32 as Win32
import Graphics.HGL.Win32.Types
import Data.Bits
#endif

import Graphics.HGL.Units (Point, Size)
import Graphics.HGL.Draw.Monad (Graphic, Draw)
import Graphics.HGL.Internals.Draw (mkDraw)
import Graphics.HGL.Internals.Types
	(RGB(..), BkMode(..), Alignment, HAlign(..), VAlign(..))

----------------------------------------------------------------
-- The Interface (SOE, p50)
----------------------------------------------------------------

-- | Render a 'String' positioned relative to the specified 'Point'.
text         :: Point -> String                  -> Graphic  -- filled

#if !X_DISPLAY_MISSING
-- | @'textInfo' s@ returns:
--
-- (1) The offset at which the string would be drawn according to the
--     current text alignment (e.g., @('Center', 'Baseline')@ will result
--     in an offset of (-width\/2,0))
--
-- (2) The size at which the text would be drawn using the current font.
--
textInfo :: String -> Draw (Point,Size)
#endif

-- | Set the foreground color for drawing text, returning the previous value.
setTextColor     :: RGB           -> Draw RGB

-- | Set the background color for drawing text, returning the previous value.
-- The background color is ignored when the mode is 'Transparent'.
setBkColor       :: RGB           -> Draw RGB

-- | Set the background mode for drawing text, returning the previous value.
setBkMode        :: BkMode        -> Draw BkMode

-- | Set the alignment for drawing text, returning the previous value.
setTextAlignment :: Alignment     -> Draw Alignment

----------------------------------------------------------------
-- The Implementation
----------------------------------------------------------------

#if !X_DISPLAY_MISSING

text p s = mkDraw (\ dc -> do
  bs <- readMVar (ref_bits dc)
  let
    Font f  = font bs
    (halign, valign) = textAlignment bs

    width   = X.textWidth f s
    ascent  = X.ascentFromFontStruct f
    descent = X.descentFromFontStruct f

    x' = case halign of
         Left'  -> x
         Center -> x - width `div` 2
         Right' -> x - width + 1
    y' = case valign of
         Top      -> y + ascent
         Baseline -> y
         Bottom   -> y - descent + 1

  draw (bkMode bs) (disp dc) (drawable dc) (textGC dc) x' y' s
 )
 where
  X.Point x y = fromPoint p

  -- Win32's DeviceContext has a BkMode in it.  In X, we call two different
  -- routines depending on what mode we want.
  draw Transparent = X.drawString
  draw Opaque      = X.drawImageString

textInfo s = mkDraw $ \ dc -> do
  bs <- readMVar (ref_bits dc)
  let
    Font f  = font bs
    (halign, valign) = textAlignment bs

    width   = X.textWidth f s
    ascent  = X.ascentFromFontStruct f
    descent = X.descentFromFontStruct f

    x1 = case halign of
          Left'  -> 0
          Center -> - width `div` 2
          Right' -> - width + 1
    y1 = case valign of
          Top      -> ascent
          Baseline -> 0
          Bottom   -> - descent + 1

    x2 = x1 + width
    y2 = y1 + ascent + descent

    (x1',x2') = (min x1 x2, max x1 x2)
    (y1',y2') = (min y1 y2, max y1 y2)

  return (toPoint (X.Point x1 y1), toSize (fromIntegral (x2'-x1'), fromIntegral (y2'-y1')))

setTextColor     x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{textColor=x}
  p <- lookupColor (disp dc) x
  X.setForeground (disp dc) (textGC dc) p
  return (textColor bs)

setBkColor       x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{bkColor=x}
  p <- lookupColor (disp dc) x
  X.setBackground (disp dc) (textGC dc) p
  return (bkColor bs)

setBkMode        x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{bkMode=x}
  return (bkMode bs)

setTextAlignment x = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{textAlignment=x}
  return (textAlignment bs)

#else /* X_DISPLAY_MISSING */

type TextAlignment = Win32.TextAlignment

fromAlignment :: Alignment -> TextAlignment
fromAlignment (ha,va) = hAlign ha .|. vAlign va

hAlign :: HAlign -> TextAlignment
hAlign Left'    = Win32.tA_LEFT       
hAlign Center  	= Win32.tA_CENTER     
hAlign Right'   = Win32.tA_RIGHT      
               
vAlign :: VAlign -> TextAlignment
vAlign Top     	= Win32.tA_TOP        
vAlign Baseline	= Win32.tA_BASELINE   
vAlign Bottom  	= Win32.tA_BOTTOM     

toAlignment :: TextAlignment -> Alignment
toAlignment x = (toHAlign (x .&. hmask), toVAlign (x .&. vmask))

toHAlign x
  | x == Win32.tA_LEFT   = Left'
  | x == Win32.tA_CENTER = Center
  | x == Win32.tA_RIGHT  = Right'
  | otherwise            = Center -- safe(?) default

toVAlign x
  | x == Win32.tA_TOP      = Top
  | x == Win32.tA_BASELINE = Baseline
  | x == Win32.tA_BOTTOM   = Bottom
  | otherwise              = Baseline -- safe(?) default

-- Win32 doesn't seem to provide the masks I need - these ought to work.
hmask = Win32.tA_LEFT .|. Win32.tA_CENTER   .|. Win32.tA_RIGHT  
vmask = Win32.tA_TOP  .|. Win32.tA_BASELINE .|. Win32.tA_BOTTOM

fromBkMode :: BkMode -> Win32.BackgroundMode
fromBkMode Opaque      = Win32.oPAQUE
fromBkMode Transparent = Win32.tRANSPARENT

toBkMode :: Win32.BackgroundMode -> BkMode
toBkMode x
  | x == Win32.oPAQUE      = Opaque
  | x == Win32.tRANSPARENT = Transparent

-- ToDo: add an update mode for these constants
-- (not required at the moment since we always specify exactly where
-- the text is to go)
-- tA_NOUPDATECP :: TextAlignment
-- tA_UPDATECP   :: TextAlignment

text (x,y) s = mkDraw $ \ hdc -> 
  Win32.textOut hdc (fromDimension x) (fromDimension y) s

setTextColor c = mkDraw (\hdc -> do
  c' <- Win32.setTextColor hdc (fromRGB c)
  return (toRGB c'))
  
setBkColor c = mkDraw (\hdc -> do
  c' <- Win32.setBkColor hdc (fromRGB c)
  return (toRGB c'))
  
setBkMode m = mkDraw (\hdc -> do
  m' <- Win32.setBkMode hdc (fromBkMode m)
  return (toBkMode m'))
  
setTextAlignment new_alignment = mkDraw (\hdc -> do
  old <- Win32.setTextAlign hdc (fromAlignment new_alignment)
  return (toAlignment old)
  )

#endif /* X_DISPLAY_MISSING */

----------------------------------------------------------------
-- End
----------------------------------------------------------------
