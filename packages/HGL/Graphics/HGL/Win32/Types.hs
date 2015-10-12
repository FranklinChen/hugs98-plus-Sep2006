-- #hide
module Graphics.HGL.Win32.Types
	( toDimension, fromDimension
	, toPoint,     fromPoint
	, toRGB,       fromRGB
	, Bitmap(..)
	, DC
	, Key(MkKey)
	) where

import qualified Graphics.Win32 as Win32
import Graphics.HGL.Internals.Types

-- Hugs does not allow operators to have different fixities in 
-- different modules (this is a known deviation from Standard Haskell).
-- In consequence, we don't declare any fixities in any non-standard
-- library because it would prevent the programmer from using the same
-- operator name at a different fixity.
--
-- infixr 9 `over`

----------------------------------------------------------------
-- Units
----------------------------------------------------------------

-- These functions are used when implementing Graphic values
toPoint        :: Win32.POINT -> Point
fromPoint      :: Point -> Win32.POINT

toDimension    :: Win32.INT -> Dimension
fromDimension  :: Dimension -> Win32.INT

toPoint       (x,y) = (toDimension x, toDimension y)
fromPoint     (x,y) = (fromDimension x, fromDimension y)
toDimension   = fromIntegral
fromDimension = fromIntegral

---------------------------------------------------------------
-- Colors
----------------------------------------------------------------

fromRGB :: RGB -> Win32.COLORREF
fromRGB (RGB r g b) = Win32.rgb r g b

toRGB :: Win32.COLORREF -> RGB
toRGB c = RGB (Win32.getRValue c) (Win32.getGValue c) (Win32.getBValue c)

----------------------------------------------------------------
-- Bitmaps
----------------------------------------------------------------

newtype Bitmap = MkBitmap Win32.HBITMAP

----------------------------------------------------------------
-- Drawing Context
----------------------------------------------------------------

type DC = Win32.HDC

newtype Key = MkKey Win32.VKey deriving Show

----------------------------------------------------------------
