-- #hide
module Graphics.HGL.Win32.Bitmap
	( Bitmap
	, load, read, delete
	, draw, drawStretched, drawSheared
	, getBitmapSize
	, createBitmapFile
	) where

import Graphics.HGL.Units (Point)
import Graphics.HGL.Internals.Draw (Draw, mkDraw)
import Graphics.HGL.Win32.Draw
import Graphics.HGL.Win32.Types
import qualified Graphics.HGL.Internals.Utilities as Utils

import qualified Graphics.Win32 as Win32
import qualified System.Win32 as Win32

import Foreign

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

load    :: String -> IO (Bitmap, (Int, Int))
delete  :: Bitmap -> IO ()

getBitmapSize :: Bitmap -> IO (Int, Int)

-- Bitmaps can be drawn in three ways:
-- a) with no transformation at  a point
-- b) stretched to fit           a rectangle
-- c) rotated and sheared to fit a parallelogram
--
-- Sadly, the latter isn't supported in Win'95

draw          :: Point                   -> Bitmap -> Draw ()
drawStretched :: Point -> Point          -> Bitmap -> Draw ()
drawSheared   :: Point -> Point -> Point -> Bitmap -> Draw ()

----------------------------------------------------------------
-- The implementation
----------------------------------------------------------------

delete (MkBitmap bitmap) = Win32.deleteBitmap bitmap

load fileName = do
     	--putStrLn ("<<DEBUG: loading bitmap \"" ++ fileName ++ "\">>")
     	bmp <- readBitmap fileName
     	sz  <- getBitmapSize bmp
     	return (bmp, sz)

getBitmapSize (MkBitmap bmp) = do
	(ty, w, h, wBytes, planes, bitsPixel, bits) <- Win32.getBitmapInfo bmp
    	return (fromIntegral w, fromIntegral h)

draw pt bmp = mkDraw (\ hdc ->
  withCompatibleDC hdc $ \ memdc ->
  withBitmap memdc bmp $ do
    (width,height) <- getBitmapSize bmp
    Win32.bitBlt hdc x y (fromIntegral width) (fromIntegral height) 
                 memdc 0 0 Win32.sRCCOPY)
 where
  (x,y) = fromPoint pt

drawStretched p0 p1 bmp = mkDraw (\hdc -> 
  withCompatibleDC hdc $ \ memdc ->
  withBitmap memdc bmp $ do
    (width,height) <- getBitmapSize bmp
    Win32.stretchBlt hdc   x0 y1 (x1-x0) (y0-y1)
	             memdc 0  0  (fromIntegral width) (fromIntegral height) Win32.sRCCOPY)
 where
  (x0,y0) = fromPoint p0
  (x1,y1) = fromPoint p1

drawSheared p0 p1 p2 bmp = mkDraw (\hdc ->
  withCompatibleDC hdc $ \ memdc ->
  withBitmap memdc bmp $ do
    (width,height) <- getBitmapSize bmp
    Win32.plgBlt hdc (fromPoint p0) (fromPoint p1) (fromPoint p2)
                     memdc 0 0 (fromIntegral width) (fromIntegral height) Nothing 0 0)

----------------------------------------------------------------
-- Reading bitmaps from files
----------------------------------------------------------------

-- ToDo: the "bits" read are never freed but I think we can free them
-- as soon as we call createDIBitmap.

-- Summary of the Win32 documentation on BMP files:
--
-- A bitmap file consists of:
-- 
-- 	   +-------------------+
-- 	   | BITMAPFILEHEADER  |
-- 	   +-------------------+
-- 	   | BITMAPINFOHEADER  |
-- 	   +-------------------+
-- 	   | Rgbquad array     |
-- 	   +-------------------+
-- 	   | Color-index array |
-- 	   +-------------------+
-- 
-- The file header tells you the size of the file and the offset of the
-- bitmap data from the header start.
-- 
-- The info header specifies the width and height, the colour format,
-- compression mode, number of bytes of data, resolution and the number
-- of colours.
-- 
-- The RGBQUAD array is a palette.
-- 
-- The Color-index array is the actual bitmap.

readBitmap fileName = 
  Utils.bracket 
    (Win32.createFile fileName 
	  	      Win32.gENERIC_READ Win32.fILE_SHARE_READ Nothing 
		      Win32.oPEN_EXISTING Win32.fILE_ATTRIBUTE_NORMAL Nothing)
    Win32.closeHandle
    $ \ file -> do
  (offset, size) <- readFileHeader file
  (infoHeader,bmi,bits) <- readBits file offset size
  hdc <- Win32.getDC Nothing  -- hdc for the screen
  bmp <- Win32.createDIBitmap hdc infoHeader Win32.cBM_INIT bits bmi Win32.dIB_RGB_COLORS
  return (MkBitmap bmp)

readFileHeader :: Win32.HANDLE -> IO (Word32, Word32)
readFileHeader file =
  -- read the file header
  allocaBytes (fromIntegral Win32.sizeofLPBITMAPFILEHEADER) $ \ fileHeader -> do
  read <- Win32.win32_ReadFile file fileHeader
            Win32.sizeofLPBITMAPFILEHEADER Nothing
  assert (read == Win32.sizeofLPBITMAPFILEHEADER) "Bitmap file lacks header"
  -- check the tag and get the size
  (tag, size, r1, r2, offset) <- Win32.getBITMAPFILEHEADER fileHeader
  assert (tag == fromIntegral (fromEnum 'B' + 256 * fromEnum 'M')) 
         "Bitmap file lacks tag"
  assert (r1 == 0 && r2 == 0) 
         "Bitmap header contains non-zero reserved words"
  return ( offset - Win32.sizeofLPBITMAPFILEHEADER
         , size - Win32.sizeofLPBITMAPFILEHEADER
         )

-- read the bits out of the rest of the file
-- assumes that you've just read the file header
readBits :: Win32.HANDLE -> Word32 -> Word32 -> 
              IO (Win32.LPBITMAPINFOHEADER, Win32.LPBITMAPINFO, Win32.LPVOID)
readBits file offset size = do
  header <- mallocBytes (fromIntegral size)
  read <- Win32.win32_ReadFile file header size Nothing
  assert (read == size) "Bitmap file ended unexpectedly"
  return ( castPtr header
         , header
         , castPtr header `plusPtr` fromIntegral offset
	 )

-- In the development system, this might print the error message
-- if the assertion fails.
assert :: Bool -> String -> IO ()
assert _ _ = return ()
{-
assert True _ = return ()
assert False why = do
  putStrLn "Assertion failed:"
  putStrLn why
  return ()
-}

----------------------------------------------------------------
-- End
----------------------------------------------------------------
