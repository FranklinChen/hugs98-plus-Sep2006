-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Run
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Running graphical actions.
--
-----------------------------------------------------------------------------

#include "HsHGLConfig.h"

module Graphics.HGL.Run
	( runGraphics		-- :: IO () -> IO ()
	) where

#if !X_DISPLAY_MISSING
import Graphics.HGL.X11.Display (getDisplayName)
import Graphics.HGL.X11.Window (runGraphicsEx)
#else
import Graphics.HGL.Win32.WND (handleEvents, beginGraphics, endGraphics)
import Graphics.HGL.Internals.Utilities (safeTry)
import Control.Concurrent (forkIO, yield)
import Data.IORef( newIORef, readIORef, writeIORef )
import System.IO.Error (try)
#endif

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- | Initialize the system to do graphics, run an action while collecting
-- user interface events and forwarding them to the action, and then clean
-- up everything else at the end.
-- The other functions of the library may only be used inside 'runGraphics'.
runGraphics :: IO () -> IO ()  -- SOE, p48

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

#if !X_DISPLAY_MISSING

runGraphics m = do
  disp <- getDisplayName
  runGraphicsEx disp m

#else /* X_DISPLAY_MISSING */

-- We took a lot of effort to make sure that we always close the
-- windows - even if "m" fails.
--
-- Note though that we use "try" instead of "safeTry" on the call to
-- "m" because it is quite normal for "m" to block (and safeTry treats
-- blocking as failure).

runGraphics m = do
  beginGraphics
  quit <- newIORef False
  safeTry $ do
    forkIO (try m >> writeIORef quit True)
    yield
    handleEvents (readIORef quit)
  endGraphics

#endif /* X_DISPLAY_MISSING */
