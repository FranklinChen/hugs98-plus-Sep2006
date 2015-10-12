-----------------------------------------------------------------------------
-- Library of escape sequences for ANSI compatible screen I/O:
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module AnsiScreen(
	Pos(..),
	cls,
	goto, at, home, 
	highlight
	) where

-- Basic screen control codes:

type Pos           = (Int,Int)

at        :: Pos -> String -> String
highlight :: String -> String
goto      :: Int -> Int -> String
home      :: String
cls       :: String

at (x,y) s  = goto x y ++ s
highlight s = "\ESC[7m"++s++"\ESC[0m"
goto x y    = '\ESC':'[':(show y ++(';':show x ++ "H"))
home        = goto 1 1

-- Choose whichever of the following lines is suitable for your system:
cls         = "\ESC[2J"     -- for PC with ANSI.SYS
--cls         = "\^L"         -- for Sun window

-----------------------------------------------------------------------------
