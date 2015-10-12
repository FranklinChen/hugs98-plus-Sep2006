{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.Clock.Scale
(
	-- * Universal Time
	-- | Time as measured by the earth.
	UniversalTime(..),

	-- * Absolute intervals
	DiffTime
) where

import Data.Fixed

-- | The Modified Julian Date is the day with the fraction of the day, measured from UT midnight.
-- It's used to represent UT1, which is time as measured by the earth's rotation, adjusted for various wobbles.
newtype UniversalTime = ModJulianDate {getModJulianDate :: Rational} deriving (Eq,Ord)

-- | This is a length of time, as measured by a clock.
-- Conversion functions will treat it as seconds.
-- It has an accuracy of 10^-12 s.
newtype DiffTime = MkDiffTime Pico deriving (Eq,Ord)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Enum DiffTime where
	succ (MkDiffTime a) = MkDiffTime (succ a)
	pred (MkDiffTime a) = MkDiffTime (pred a)
	toEnum = MkDiffTime . toEnum
	fromEnum (MkDiffTime a) = fromEnum a
	enumFrom (MkDiffTime a) = fmap MkDiffTime (enumFrom a)
	enumFromThen (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromThen a b)
	enumFromTo (MkDiffTime a) (MkDiffTime b) = fmap MkDiffTime (enumFromTo a b)
	enumFromThenTo (MkDiffTime a) (MkDiffTime b) (MkDiffTime c) = fmap MkDiffTime (enumFromThenTo a b c)

instance Show DiffTime where
	show (MkDiffTime t) = (showFixed True t) ++ "s"

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Num DiffTime where
	(MkDiffTime a) + (MkDiffTime b) = MkDiffTime (a + b)
	(MkDiffTime a) - (MkDiffTime b) = MkDiffTime (a - b)
	(MkDiffTime a) * (MkDiffTime b) = MkDiffTime (a * b)
	negate (MkDiffTime a) = MkDiffTime (negate a)
	abs (MkDiffTime a) = MkDiffTime (abs a)
	signum (MkDiffTime a) = MkDiffTime (signum a)
	fromInteger i = MkDiffTime (fromInteger i)

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Real DiffTime where
	toRational (MkDiffTime a) = toRational a

-- necessary because H98 doesn't have "cunning newtype" derivation
instance Fractional DiffTime where
	(MkDiffTime a) / (MkDiffTime b) = MkDiffTime (a / b)
	recip (MkDiffTime a) = MkDiffTime (recip a)
	fromRational r = MkDiffTime (fromRational r)
