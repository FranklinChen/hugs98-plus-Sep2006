-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Draw.Font
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Text fonts.
--
-- Portability notes:
--
-- * X11 does not directly support font rotation so 'createFont' and
--   'mkFont' always ignore the rotation angle argument in the X11
--   implementation of this library.
--
-- * Many of the font families typically available on Win32 are not
--   available on X11 (and /vice-versa/).  In our experience, the font
--   families /courier/, /helvetica/ and /times/ are somewhat portable.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

module Graphics.HGL.Draw.Font
	( Font
	, createFont
	, deleteFont
	, selectFont		-- :: Font -> Draw Font
	, mkFont
	) where

#if !X_DISPLAY_MISSING
import qualified Graphics.HGL.Internals.Utilities as Utils
import Graphics.HGL.X11.Types (Font(Font), DC(..), DC_Bits(..))
import Graphics.HGL.X11.Display (getDisplay)
import qualified Graphics.X11.Xlib as X
import Control.Concurrent.MVar (takeMVar, putMVar)
#else
import Graphics.HGL.Win32.Types
import qualified Graphics.Win32 as Win32
#endif

import Graphics.HGL.Units (Size, Angle)
import Graphics.HGL.Draw.Monad (Draw, bracket, ioToDraw)
import Graphics.HGL.Internals.Draw (mkDraw)

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

#if X_DISPLAY_MISSING
newtype Font = MkFont Win32.HFONT
#endif

-- | Create a font.
-- The rotation angle is ignored if the font is not a \"TrueType\" font
-- (e.g., a @System@ font on Win32).
createFont
	:: Size		-- ^ size of character glyphs in pixels
	-> Angle	-- ^ rotation angle
	-> Bool		-- ^ bold font?
	-> Bool		-- ^ italic font?
	-> String	-- ^ font family
	-> IO Font

-- | Delete a font created with 'createFont'.
deleteFont :: Font -> IO ()

-- | Set the font for subsequent text, and return the previous font.
selectFont :: Font -> Draw Font  

-- | Generate a font for use in a drawing, and delete it afterwards.
-- The rotation angle is ignored if the font is not a \"TrueType\" font
-- (e.g., a @System@ font on Win32).
mkFont	:: Size		-- ^ size of character glyphs in pixels
	-> Angle	-- ^ rotation angle
	-> Bool		-- ^ bold font?
	-> Bool		-- ^ italic font?
	-> String	-- ^ font family
	-> (Font  -> Draw a)
	-> Draw a

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

mkFont size angle bold italic family =
  bracket (ioToDraw $ createFont size angle bold italic family)
          (ioToDraw . deleteFont)

#if !X_DISPLAY_MISSING

createFont (width, height) escapement bold italic family = do
  display <- getDisplay
--  print fontName
  r <- Utils.safeTry (X.loadQueryFont display fontName)
  case r of
    Left e  -> ioError (userError $ "Unable to load font " ++ fontName)
    Right f -> return (Font f)
 where
  fontName = concatMap ('-':) fontParts
  fontParts = [ foundry
              , family
              , weight
              , slant
              , sWdth
              , adstyl
              , pxlsz
              , ptSz
              , resx
              , resy
              , spc
              , avgWidth
              , registry
              , encoding
              ]
  foundry  = "*" -- eg "adobe"
  -- family   = "*" -- eg "courier"
  weight   = if bold then "bold" else "medium"
  slant    = if italic then "i" else "r"
  sWdth    = "normal"
  adstyl   = "*"
  pxlsz    = show height
  ptSz     = "*"
  resx     = "75"
  resy     = "75"
  spc      = "*"
  avgWidth = show (width*10) -- not sure what unit they use
  registry = "*"
  encoding = "*"

deleteFont (Font f) = do
  display <- getDisplay
  X.freeFont display f

selectFont f@(Font x) = mkDraw $ \ dc -> do
  bs <- takeMVar (ref_bits dc)
  putMVar (ref_bits dc) bs{font=f}
  X.setFont (disp dc) (textGC dc) (X.fontFromFontStruct x)
  return (font bs)

#else /* X_DISPLAY_MISSING */

createFont (width, height) escapement bold italic family = 
 Win32.createFont (fromDimension height) (fromDimension width)
                  (round (escapement * 1800/pi))
                  0                     -- orientation
                  weight
                  italic False False    -- italic, underline, strikeout
                  Win32.aNSI_CHARSET
                  Win32.oUT_DEFAULT_PRECIS
                  Win32.cLIP_DEFAULT_PRECIS
                  Win32.dEFAULT_QUALITY
                  Win32.dEFAULT_PITCH
                  family
  >>= return . MkFont
 where
  weight | bold      = Win32.fW_BOLD
         | otherwise = Win32.fW_NORMAL

deleteFont (MkFont f) = Win32.deleteFont f

selectFont (MkFont f) = mkDraw (\hdc -> do
  f' <- Win32.selectFont hdc f
  return (MkFont f'))

#endif /* X_DISPLAY_MISSING */

----------------------------------------------------------------
-- End
----------------------------------------------------------------
