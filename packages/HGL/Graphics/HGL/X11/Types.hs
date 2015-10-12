-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.Types
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- Basic types for a simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.X11.Types
	( DC(..)
	, DC_Bits(..)
	, Font(Font), Brush(Brush), Pen(Pen), defaultPen
	, Key(MkKey)
	, fromPoint, toPoint
	, fromSize,  toSize
	, lookupColor
	) where

import Graphics.HGL.Internals.Types

import qualified Graphics.X11.Xlib as X

import Control.Concurrent.MVar (MVar)
import Data.Bits
import Data.Word (Word8)

----------------------------------------------------------------
-- Units
----------------------------------------------------------------

fromPoint :: Point -> X.Point
toPoint   :: X.Point -> Point
fromSize  :: Size -> (X.Dimension, X.Dimension)
toSize    :: (X.Dimension, X.Dimension) -> Size

fromPoint (x,y) = X.Point (fromIntegral x) (fromIntegral y)
toPoint   (X.Point x y) = (fromIntegral x, fromIntegral y)
fromSize  (x,y) = (fromIntegral x, fromIntegral y)
toSize    (x,y) = (fromIntegral x, fromIntegral y)

----------------------------------------------------------------
-- Device Context (simulates Win32 Device Contexts)
----------------------------------------------------------------

data DC = MkDC
  { disp     :: X.Display
  , drawable :: X.Drawable
  , textGC   :: X.GC
  , paintGC  :: X.GC
  , brushGC  :: X.GC
  , ref_rect :: MVar (X.Point,(X.Dimension, X.Dimension))
  , ref_bits :: MVar DC_Bits
  }

data DC_Bits = DC_Bits
  { textColor     :: RGB
  , bkColor       :: RGB
  , bkMode        :: BkMode
  , textAlignment :: Alignment
  , brush         :: Brush
  , pen           :: Pen
  , font          :: Font
  }

newtype Key = MkKey X.KeySym deriving Show

newtype Font = Font X.FontStruct
newtype Brush = Brush RGB

data Pen = Pen Style Int X.Pixel

defaultPen :: X.Pixel -> Pen
defaultPen col = Pen Solid 0 col

lookupColor :: X.Display -> RGB -> IO X.Pixel
lookupColor display col = (do
  (X.Color p _ _ _ _) <-
      X.allocColor display color_map (X.Color 0 r g b xcolor_flags)
  return p)
     `catch` \ err -> 
               print err >> return 0
--	       ioError (userError ("Error: " ++ show err
--			      ++ "\nUnable to allocate colo[u]r " ++ show (r,g,b) 
--			      ++ " - I'll bet you're running Netscape."))
 where
  screen    = X.defaultScreenOfDisplay display
  color_map = X.defaultColormapOfScreen screen

  RGB r' g' b' = col
  (r,g,b) = ((fromIntegral r') * 256, (fromIntegral g') * 256, (fromIntegral b')*256)

xcolor_flags :: Word8
xcolor_flags = X.doRed .|. X.doGreen .|. X.doBlue
