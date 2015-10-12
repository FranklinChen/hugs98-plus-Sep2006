-- #hide
module Graphics.HGL.Win32.Draw
	( DrawFun, drawGraphic, drawBufferedGraphic, drawBufferedGraphicBC
	, saveGraphic
	, withBitmap
	, setDefaults
	, createCompatibleBitmap, withCompatibleBitmap, withCompatibleDC, withDC
	, createBitmapFile
	) where

import Graphics.HGL.Units
import Graphics.HGL.Internals.Draw
import Graphics.HGL.Win32.Types
import qualified Graphics.HGL.Internals.Utilities as Utils
import qualified Graphics.Win32 as Win32
import Data.Int

type DrawFun = Win32.HWND -> Win32.HDC -> IO ()

drawGraphic            ::                    Draw () -> DrawFun
drawBufferedGraphic    ::                    Draw () -> DrawFun
drawBufferedGraphicBC  :: Win32.COLORREF  -> Draw () -> DrawFun
saveGraphic            :: String -> Point -> Draw () -> IO ()

createBitmapFile       :: Win32.HDC -> String -> Bitmap -> IO ()
createCompatibleDC     :: Win32.HDC -> IO Win32.HDC
deleteDC               :: Win32.HDC -> IO ()
createCompatibleBitmap :: Win32.HDC -> Int32 -> Int32 -> IO Bitmap
withCompatibleDC       :: Win32.HDC -> (Win32.HDC -> IO a) -> IO a
withBitmap             :: Win32.HDC -> Bitmap -> IO a -> IO a
withDC                 :: Maybe Win32.HWND -> (Win32.HDC -> IO a) -> IO a
withCompatibleBitmap   :: Win32.HDC -> Int32 -> Int32 -> (Bitmap -> IO a) -> IO a

----------------------------------------------------------------

drawGraphic p = \ hwnd hdc -> do
  (w,h) <- windowSize hwnd
  Win32.bitBlt hdc 0 0 w h hdc 0 0 backgroundColor
  setDefaults hdc
  unDraw p hdc

drawBufferedGraphic = drawBufferedGraphicBC backgroundColor

drawBufferedGraphicBC bgColor p = \ hwnd hdc -> do
  (w,h) <- windowSize hwnd
  withBuffer (Just hwnd) w h bgColor $ \ buffer _ -> do
    setDefaults buffer
    unDraw p buffer
    Win32.bitBlt hdc 0 0 w h buffer 0 0 Win32.sRCCOPY

saveGraphic fileName size p = 
  withBuffer Nothing w h backgroundColor $ \ buffer bmp -> do
    setDefaults buffer
    unDraw p buffer
    createBitmapFile buffer fileName bmp
 where
  (w,h) = fromPoint size

backgroundColor = Win32.bLACKNESS

setDefaults :: Win32.HDC -> IO ()
setDefaults hdc = do
  setDefaultPen   hdc
  setDefaultBrush hdc
  setDefaultText  hdc
  return ()

setDefaultPen :: Win32.HDC -> IO ()
setDefaultPen = \ hdc -> do
  whitePen <- Win32.getStockPen Win32.wHITE_PEN
  Win32.selectPen hdc whitePen
  return ()

setDefaultBrush :: Win32.HDC -> IO ()
setDefaultBrush = \ hdc -> do
  whiteBrush <- Win32.getStockBrush Win32.wHITE_BRUSH
  Win32.selectBrush hdc whiteBrush
  return ()

setDefaultText :: Win32.HDC -> IO ()
setDefaultText = \ hdc -> do
  Win32.setTextColor hdc white
-- We omit this because it should be redundant (since mode is transparent)
-- And because it causes some examples to crash.
-- Maybe you're not allowed to set a color if the mode is transparent?
--  Win32.setBkColor   hdc black
  Win32.setBkMode    hdc Win32.tRANSPARENT
  return ()

white :: Win32.COLORREF
white = Win32.rgb 255 255 255

black :: Win32.COLORREF
black = Win32.rgb 0 0 0

----------------------------------------------------------------
-- Note that we create a bitmap which is compatible with the hdc
-- onto which we are going to zap the Graphic.  It might seem that
-- it would be enough for it to be compatible with the buffer -
-- but, sadly, this isn't the case.  The problem is that the buffer
-- is initially 0 pixels wide, 0 pixels high and 1 bit deep
-- (ie it looks monochrome); it only becomes n-bits deep when you
-- select in a bitmap which is n-bits deep.
--
-- If it wasn't for that, we'd have swapped these two lines:
--
--   withCompatibleBitmap w h   $ \ bitmap ->
--   withCompatibleDC           $ \ hdc    ->
--
withBuffer :: Maybe Win32.HWND -> Int32 -> Int32 -> Win32.COLORREF -> (Win32.HDC -> Bitmap -> IO a) -> IO a

withBuffer mbhwnd w h bgColor p = 
  withDC mbhwnd                $ \ hdc    -> 
  withCompatibleBitmap hdc w h $ \ bitmap ->
  withCompatibleDC hdc         $ \ buffer ->
  withBitmap buffer bitmap     $ do
    Win32.bitBlt buffer 0 0 w h buffer 0 0 bgColor
    p buffer bitmap

----------------------------------------------------------------

-- Get the width and height of a window's client area, in pixels.
windowSize :: Win32.HWND -> IO (Win32.LONG,Win32.LONG)
windowSize hwnd =
 Win32.getClientRect hwnd >>= \ (l',t',r',b') ->
 return (r' - l', b' - t')

-- Note that this DC is only "1 bit" in size - you have to call
-- "createCompatibleBitmap" before it is big enough to hold the bitmap
-- you want.
createCompatibleDC hdc = Win32.createCompatibleDC (Just hdc)

deleteDC = Win32.deleteDC

createCompatibleBitmap hdc w h = do
  bmp <- Win32.createCompatibleBitmap hdc w h
  return (MkBitmap bmp)

withBitmap hdc bmp = Utils.bracket_ (selectBitmap hdc bmp) (selectBitmap hdc)

withDC mhwnd = Utils.bracket (Win32.getDC mhwnd) (Win32.releaseDC mhwnd)

-- Note that this DC is only "1 bit" in size - you have to call
-- "createCompatibleBitmap" before it is big enough to hold the bitmap
-- you want.
withCompatibleDC hdc = Utils.bracket (createCompatibleDC hdc) deleteDC

withCompatibleBitmap hdc w h =
  Utils.bracket (createCompatibleBitmap hdc w h) deleteBitmap

deleteBitmap (MkBitmap bmp) = Win32.deleteBitmap bmp

selectBitmap hdc (MkBitmap bmp) = do
  bmp' <- Win32.selectBitmap hdc bmp
  return (MkBitmap bmp)

createBitmapFile hdc fileName (MkBitmap bmp) = 
  Win32.createBMPFile fileName bmp hdc

----------------------------------------------------------------
-- End
----------------------------------------------------------------
