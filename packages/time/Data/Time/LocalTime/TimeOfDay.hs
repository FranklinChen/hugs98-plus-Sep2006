{-# OPTIONS -Wall -Werror #-}

-- #hide
module Data.Time.LocalTime.TimeOfDay
(
	-- * Time of day
	TimeOfDay(..),midnight,midday,
	utcToLocalTimeOfDay,localToUTCTimeOfDay,
	timeToTimeOfDay,timeOfDayToTime,
	dayFractionToTimeOfDay,timeOfDayToDayFraction
) where

import Data.Time.LocalTime.TimeZone
import Data.Time.Calendar.Private
import Data.Time.Clock
import Data.Fixed

-- | Time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day.
data TimeOfDay = TimeOfDay {
	-- | range 0 - 23
	todHour    :: Int,
	-- | range 0 - 59
	todMin     :: Int,
	-- | Note that 0 <= todSec < 61, accomodating leap seconds.
	-- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
	todSec     :: Pico
} deriving (Eq,Ord)

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

-- | Hour twelve
midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
	show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

-- | Convert a ToD in UTC to a ToD in some timezone, together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24),TimeOfDay (mod h' 24) (mod m' 60) s) where
	m' = m + timeZoneMinutes zone
	h' = h + (div m' 60)

-- | Convert a ToD in some timezone to a ToD in UTC, together with a day adjustment.
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer,TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

-- | Get a TimeOfDay given a time since midnight.
-- Time more than 24h will be converted to leap-seconds.
timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt | dt >= posixDayLength = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDayLength)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s where
	s' = realToFrac dt
	s = mod' s' 60
	m' = div' s' 60
	m = mod' m' 60
	h = div' m' 60

-- | Find out how much time since midnight a given TimeOfDay is.
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | Get a TimeOfDay given the fraction of a day since midnight.
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

-- | Get the fraction of a day since midnight given a TimeOfDay.
timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod / posixDayLength)
