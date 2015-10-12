-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Internals.Event
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- Events in a simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.Internals.Event
	( Event(..)
	-- , Event(Char,Key,Button,MouseMove,Resize,Closed) -- deriving(Show)
	-- , char		-- :: Event -> Char
	-- , keysym		-- :: Event -> Key
	-- , isDown		-- :: Event -> Bool
	-- , pt			-- :: Event -> Point
	-- , isLeft		-- :: Event -> Bool
	) where

import Graphics.HGL.Key (Key)
import Graphics.HGL.Internals.Types (Point)

-- We probably need a lot more info about the event 
-- but this will do for now.

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- Note: The Char event is for delivering properly translated characters
-- after a key*press*. At least under X, a single key press might yield
-- 0, 1 or more characters after translation (see X[mb]LookupString).
-- The Key event is intended for reporting key up/down events of
-- *abstract* keys, i.e. KeySyms rather than KeyCodes in X terms.
-- To make it possible to report such events for arrow keys, function
-- keys and the like, the Char field needs to be replaced by a field of
-- a type somewhat isomorphic to KeySym, but valid under Windows too.

-- | A user interface event.
--
-- Notes:
--
-- * Programmers should assume that the 'Event' datatype will be
--   extended in the not-too-distant future and that individual events
--   may change slightly.  As a minimum, you should add a \"match anything\"
--   alternative to any function which pattern matches against 'Event's.
--
-- * X11 systems typically have three button mice.  Button 1 is used as the
--   left button, button 3 as the right button and button 2 (the middle
--   button) is ignored.

data Event 
  = Char
    { char :: Char	-- ^ the character represented by a key combination
    }			-- ^ a properly translated character, sent after
			-- a key press.
  | Key
    { keysym :: Key	-- ^ representation of the keyboard keys pressed
    , isDown :: Bool	-- ^ if 'True', the key was pressed;
			-- otherwise it was released
    }			-- ^ occurs when a key was pressed or released.
  | Button
    { pt :: Point	-- ^ the position of the mouse cursor
    , isLeft :: Bool	-- ^ if 'True', it was the left button
    , isDown :: Bool	-- ^ if 'True', the button was pressed;
			-- otherwise it was released
    }			-- ^ occurs when a mouse button is pressed or released.
  | MouseMove
    { pt :: Point	-- ^ the position of the mouse cursor after the movement
    }			-- ^ occurs when the mouse is moved inside the window.
  | Resize		-- ^ occurs when the window is resized.
  | Closed		-- ^ occurs when the window is closed.
 deriving Show 

----------------------------------------------------------------
-- End
----------------------------------------------------------------
