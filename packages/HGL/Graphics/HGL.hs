-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

module Graphics.HGL
        (
	  -- $intro
	  module Graphics.HGL.Units
	, module Graphics.HGL.Run
	, module Graphics.HGL.Window
	, module Graphics.HGL.Draw
	, module Graphics.HGL.Key
        , module Graphics.HGL.Utils
	  -- $utils
        ) where

import Graphics.HGL.Units
import Graphics.HGL.Run
import Graphics.HGL.Window
import Graphics.HGL.Draw
import Graphics.HGL.Key
import Graphics.HGL.Utils

{- $intro

The Haskell Graphics Library is designed to give the programmer access
to most interesting parts of the Win32 Graphics Device Interface and X11
library without exposing the programmer to the pain and anguish usually
associated with using these interfaces.

To give you a taste of what the library looks like, here is the 
obligatory \"Hello World\" program:

> module Main where
>
> import Graphics.HGL
>
> main :: IO ()
> main = runGraphics $
>	withWindow_ "Hello World Window" (300, 200) $ \ w -> do
>	drawInWindow w $ text (100, 100) "Hello World"
>	drawInWindow w $ ellipse (100, 80) (200, 180)
>	getKey w

Here's what each function does:

* 'runGraphics' (defined in "Graphics.HGL.Run") runs a graphical action
  in an appropriate environment.  All the other functions of the library
  should be used inside 'runGraphics'.

* 'withWindow_' runs an action using a new 'Window', specifying the
  window title and size (300 pixels wide and 200 high).  The window is
  closed when the action finishes.

* 'drawInWindow' draws a 'Graphic' (an abstract representation of a
  picture) on a 'Window'.

* 'text' creates a 'Graphic' consisting of a string at the specified
  position.

* 'ellipse' creates a 'Graphic' consisting of an ellipse fitting inside
  a rectangle defined by the two points.  These and other functions for
  defining, combining and modifying pictures are in "Graphics.HGL.Draw".

* 'getKey' waits for the user to press (and release) a key.  (This is
  necessary here to prevent the window from closing before you have a
  chance to read what's on the screen.)

The library is broken up into several pieces.

-}

{- $utils

The module "Graphics.HGL.Utils" defines a number of convenience functions
in terms of more primitive functions defined by other modules.  For example,

* 'withWindow_' is defined using 'openWindowEx' and 'closeWindow' (from
  "Graphics.HGL.Window").

* 'getKey' is defined using 'getWindowEvent', which waits for a range
  of user interface events.

* Instead of drawing several 'Graphic' objects sequentially as in the
  above example, you can combine them into a single 'Graphic' object using
  'overGraphic'.

-}
