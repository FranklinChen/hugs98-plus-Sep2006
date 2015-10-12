% -----------------------------------------------------------------------------
% $Id: TimeExts.lhs,v 1.3 2004/10/06 11:16:40 ross Exp $
{-
   TimeExts.lhs implements more useful time differences for Glasgow
   Haskell times.  Time differences can be in picoseconds, seconds,
   minutes, hours, days, months or years.  So for example you can
   add/subtract N months to a date, or find someone's age in days
   given the current date and their date of birth.

   Note on Timezones
   -----------------

   We use UTC where necessary.  This will 
   occasionally matter.  For example if it is 1am April 1st
   local time and 11pm March 31st UTC, then adding 1 month will give 
   "11pm April 31st UTC", which will get rolled over to
   "11pm May 1st UTC", or "1am May 2nd local time", assuming a 
   constant time difference between local time and UTC.  
   Adding 1 month in local time would instead give "1am May 1st local
   time".  It would not be too hard to use local time, but (a)
   I doubt if anyone will really notice the difference; (b) 
   this sort of thing really ought not to be done unless Haskell
   has a proper notion of time-zones; (c) I'm not quite sure what
   to do about daylight saving time.
 
   -}

\begin{code}
module TimeExts
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
   (
   DiffPico(..),
   DiffSecond(..),
   DiffMinute(..),
   DiffHour(..),
   DiffDay(..),
   DiffMonth(..),
   DiffYear(..),

   TimeAddable,

   addClock,
   diffClock,
 
   addClockPico,
   diffClockPico,
   addClockSecond,
   diffClockSecond,
   addClockMinute,
   diffClockMinute,
   addClockHour,
   diffClockHour,
   addClockDay,
   diffClockDay,
   addClockMonth,
   diffClockMonth,
   addClockYear,
   diffClockYear
   )
   where

import System.Time

-- Time difference types
data DiffPico = DiffPico Integer
data DiffSecond = DiffSecond Integer
data DiffMinute = DiffMinute Integer
data DiffHour = DiffHour Int
-- 2^31 hours is more than 200000 years so Int is probably enough.
data DiffDay = DiffDay Int 
data DiffMonth = DiffMonth Int
data DiffYear = DiffYear Int

-- this class is implemented for each of the above types.
class TimeAddable diffType where
   addClock :: ClockTime -> diffType -> ClockTime
   -- add given time difference.  Where necessary, lower fields
   -- are rolled over to higher ones.  For example
   -- adding 1 month to March 31st gives May 1st.
   diffClock :: ClockTime -> ClockTime -> diffType
   -- diffClock timeTo timeFrom
   -- returns the time difference from timeFrom to timeTo.
   -- for example, if diffType is DayDiff,
   -- diffClock ("23:00:00 on January 2nd") ("00:00:00 on January 1st") 
   -- will be "DayDiff 1", since 1 whole day (plus a bit extra) has
   -- elapsed from the second date to the first.

-- For those who don't like the overloading in the above, we also
-- provide monomorphic versions of each of these functions for each type.
addClockPico :: ClockTime -> DiffPico -> ClockTime
diffClockPico :: ClockTime -> ClockTime -> DiffPico

addClockSecond :: ClockTime -> DiffSecond -> ClockTime
diffClockSecond :: ClockTime -> ClockTime -> DiffSecond

addClockMinute :: ClockTime -> DiffMinute -> ClockTime
diffClockMinute :: ClockTime -> ClockTime -> DiffMinute

addClockHour :: ClockTime -> DiffHour -> ClockTime
diffClockHour :: ClockTime -> ClockTime -> DiffHour

addClockDay :: ClockTime -> DiffDay -> ClockTime
diffClockDay :: ClockTime -> ClockTime -> DiffDay

addClockMonth :: ClockTime -> DiffMonth -> ClockTime
diffClockMonth :: ClockTime -> ClockTime -> DiffMonth

addClockYear :: ClockTime -> DiffYear -> ClockTime
diffClockYear :: ClockTime -> ClockTime -> DiffYear

--- END OF SPECIFICATION

instance TimeAddable DiffPico where
   addClock = addClockPico
   diffClock = diffClockPico


instance TimeAddable DiffSecond where
   addClock = addClockSecond
   diffClock = diffClockSecond


instance TimeAddable DiffMinute where
   addClock = addClockMinute
   diffClock = diffClockMinute


instance TimeAddable DiffHour where
   addClock = addClockHour
   diffClock = diffClockHour


instance TimeAddable DiffDay where
   addClock = addClockDay
   diffClock = diffClockDay


instance TimeAddable DiffMonth where
   addClock = addClockMonth
   diffClock = diffClockMonth

instance TimeAddable DiffYear where
   addClock = addClockYear
   diffClock = diffClockYear

-- Now we have to implement these functions.  We have two strategies.
-- (1) For DiffPico and DiffSecond this can be done trivially
--     by extracting the fields of the ClockTime type, which gives
--     seconds and picoseconds directly.
-- (2) For other types we convert to CalendarTime and use
--     Gregorian calendar calculations.

{-
   DiffPico
 -}

nPicos = 1000000000000

addClockPico (TOD seconds picos) (DiffPico diffPicos) =
   let
      (diffSeconds,diffRestPicos) = divMod diffPicos nPicos
      seconds' = seconds + diffSeconds
      picos' = picos + diffRestPicos
      (seconds'',picos'') =
         if picos' >= nPicos
         then
            (seconds'+1,picos'-nPicos)
         else
            (seconds',picos')
   in
      TOD seconds'' picos''

diffClockPico (TOD secondsTo picosTo) (TOD secondsFrom picosFrom) =
   DiffPico((picosTo-picosFrom) + nPicos * (secondsTo - secondsFrom))

{- 
   DiffSecond
   -}

addClockSecond (TOD seconds picos) (DiffSecond diffSeconds) =
   TOD (seconds + diffSeconds) picos

diffClockSecond (TOD secondsTo picosTo) (TOD secondsFrom picosFrom) =
   DiffSecond(if picosTo >= picosFrom
      then
         (secondsTo - secondsFrom)
      else -- borrow
         (secondsTo - secondsFrom - 1)
      )
{- 
   DiffMinute 
   -}
-- The remaining functions use the Gregorian Calendar code which
-- is shoved down to the end of this file.
addClockMinute clockTo (DiffMinute diffMinutes) =
   let
      calTo @ (CalendarTime 
        {ctYear=ctYear,ctMonth=ctMonth,ctDay=ctDay,
         ctHour=ctHour,ctMin=ctMin}) = 
         toUTCTime clockTo
      -- we will leave the other fields unchanged and hope that
      -- toClockTime will ignore them.  (Which it does, for GHC.)
      (diffHours',diffRestMinutes) = divMod diffMinutes 60
      minute' = ctMin + fromInteger diffRestMinutes
      (diffHours,minute) = 
         if minute'<60
            then
               (diffHours',minute')
            else
               (diffHours'+1,minute'-60)
      (diffDays',diffRestHours) = divMod diffHours 24
      hour' = ctHour + fromInteger diffRestHours
      (diffDays,hour) =
         if hour'<24
            then
               (diffDays',hour')
            else
               (diffDays+1,hour'-24)
      (year,month,day) =
         addDateDays (ctYear,ctMonth,ctDay) (fromInteger diffDays)
   in
      toClockTime (calTo
        {ctYear=year,ctMonth=month,ctDay=day,
         ctHour=hour,ctMin=minute
         })

diffClockMinute clockTo clockFrom =
   let
      CalendarTime {ctYear=toYear,ctMonth=toMonth,ctDay=toDay,ctHour=toHour,
         ctMin=toMinute,ctSec=toSec,ctPicosec=toPicosec} = 
            toUTCTime clockTo
      CalendarTime {ctYear=fromYear,ctMonth=fromMonth,ctDay=fromDay,
         ctHour=fromHour,ctMin=fromMinute,ctSec=fromSec,
            ctPicosec=fromPicosec} = toUTCTime clockFrom
      borrow = (toSec,toPicosec) < (fromSec,toPicosec)
      diff' = (24*60) * (toInteger (diffDates (toYear,toMonth,toDay) 
         (fromYear,fromMonth,fromDay))) + 60*(toInteger(toHour-fromHour)) + 
         toInteger(toMinute-fromMinute)
   in
      DiffMinute(if borrow then diff'-1 else diff')

{- 
   DiffHour
   We're lazy and just call the minute functions for hours and days.
   -}
addClockHour clockTo (DiffHour diffHours) =
   addClockMinute clockTo (DiffMinute (60*(toInteger diffHours)))

diffClockHour clockTo clockFrom =
   let
      DiffMinute diffMinutes = diffClockMinute clockTo clockFrom
   in
      DiffHour(fromInteger(diffMinutes `div` 60))

{- 
   DiffDay
   We're lazy and just call the minute functions for hours and days.
   For days at least this involves unnecessary multiplication and
   division, unless the compiler is very clever.
   -}
addClockDay clockTo (DiffDay diffDays) =
   addClockMinute clockTo (DiffMinute ((24*60)*(toInteger diffDays)))

diffClockDay clockTo clockFrom =
   let
      DiffMinute diffMinutes = diffClockMinute clockTo clockFrom
   in
      DiffDay(fromInteger(diffMinutes `div` (24*60)))

{- 
   DiffMonth
   Here we assume that toClockTime will roll over illegal dates,
   as when you add 1 month to March 31st and get April 31st.
   This is avoidable by doing some Gregorian calendar calculations; 
   the equivalent situation when you roll over a leap second is 
   not.
   -}
addClockMonth clockTo (DiffMonth diffMonths) =
   let
      calTo @ (CalendarTime 
        {ctYear=ctYear,ctMonth=ctMonth}) = 
         toUTCTime clockTo
      mn = (fromEnum ctMonth) + diffMonths
      (yearDiff,monthNo) = divMod mn 12
   in
      toClockTime(calTo {ctYear=ctYear+yearDiff,ctMonth=toEnum monthNo})

diffClockMonth clockTo clockFrom =
   let
      CalendarTime {ctYear=toYear,ctMonth=toMonth,ctDay=toDay,ctHour=toHour,
         ctMin=toMinute,ctSec=toSec,ctPicosec=toPicosec} = 
            toUTCTime clockTo
      CalendarTime {ctYear=fromYear,ctMonth=fromMonth,ctDay=fromDay,
         ctHour=fromHour,ctMin=fromMinute,ctSec=fromSec,
            ctPicosec=fromPicosec} = toUTCTime clockFrom
      borrow = 
         -- hack around GHC failure to order tuples with
         -- more than 5 elements.
         (toDay,toHour,toMinute,toDay,(toSec,toPicosec)) <
         (fromDay,fromHour,fromMinute,fromDay,(fromSec,fromPicosec))
      diff' = 12*(toYear-fromYear) + 
         (fromEnum toMonth - fromEnum fromMonth)
   in
      DiffMonth(if borrow then diff' -1 else diff')

{- 
   DiffYear
   It's getting late so waste CPU time/leave it to the
   compiler and use the month functions
   -}
addClockYear clockTo (DiffYear diffYears) =
   addClockMonth clockTo (DiffMonth (12*diffYears))

diffClockYear clockTo clockFrom =
   let
      DiffMonth diffMonths = diffClockMonth clockTo clockFrom
   in
      DiffYear(diffMonths `div` 12)

{- Magic code for implementing the Gregorian Calendar -}
     
-- Here are two ways of representing a date
type Date = (Int,Month,Int) -- year, month, day
type NDays = Int 
-- Counts days starting at 1st March Year 0 
-- (in the Gregorian Calendar).  So the 1st March Year 0 is
-- day 0, and so on.  We start years at March as that means
-- leap days always come at the end. 

-- The difficult bit of this module is converting from Date to
-- NDays.  We do this by going via a YDPair:
type YDPair = (Int,Int)
-- a YDPair is the number of whole years since 0th March Year 0
-- plus the number of days after that.  So the YDPair for
-- 29th Feb 2000 is (1999,360) and the YDPair for 1st Mar 2000 is
-- (2000,0).

addDateDays date n =
   nDaysToDate ( dateToNDays date + n)
diffDates dateTo dateFrom =
   (dateToNDays dateTo - dateToNDays dateFrom)

dateToNDays = ydPairToNDays . dateToYDPair
nDaysToDate = ydPairToDate . nDaysToYDPair

ydPairToNDays :: YDPair -> NDays
ydPairToNDays (years,days) =
   days + years * 365 + (years `div` 4) - (years `div` 100) + 
      (years `div` 400)

nDaysToYDPair :: NDays -> YDPair
nDaysToYDPair ndays = -- there must be a neater way of writing this!
   (400*q + 100*r + 4*s + t,days) 
   where
      -- the idea being that 0<=r<4, 0<=s<25, 0<=t<4,
      -- and so ndays = q*qd + r*rd + s*sd + t*td + days
      -- where days is as small as possible while still being non-negative.
      qd = 4*rd +1   -- days in 400 years
      rd = 25*sd - 1 -- days in 100 years
      sd = 4*td + 1 -- days in 4 years
      td = 365 -- days in 1 year.
      
      (q,qrest) = divMod ndays qd
      (r',rrest) = divMod qrest rd
      (s',srest) = divMod rrest sd
      (t',days')  = divMod srest td
       
      -- r',s',t',days' are not quite the right values of r,s,t if there's
      -- a leap day, which gives rise to d=0 and r=4 or t=4.
      (r,s,t,days) = 
         if days'/=0 
         then
            (r',s',t',days')
         else -- March 1st or leap day
            if t'==4
            then
               -- leap day
               (r',s',3,365)
            else if r'==4
            then
               -- leap day of year divisible by 400
               (3,24,3,365)
            else -- March 1st
               (r',s',t',days')

-- magic numbers to subtract from a day number in a year
-- (remember March 1st is day 0) to get a date in a month.
nMarch     =   -1
nApril     =   30
nMay       =   60
nJune      =   91
nJuly      =  121
nAugust    =  152
nSeptember =  183
nOctober   =  213
nNovember  =  244
nDecember  =  274
nJanuary   =  305
nFebruary  =  336

dateToYDPair :: Date -> YDPair
dateToYDPair (year,month,date) =
   case month of
      March     -> (year,date+nMarch)
      April     -> (year,date+nApril)
      May       -> (year,date+nMay)
      June      -> (year,date+nJune)
      July      -> (year,date+nJuly)
      August    -> (year,date+nAugust)
      September -> (year,date+nSeptember)
      October   -> (year,date+nOctober)
      November  -> (year,date+nNovember)
      December  -> (year,date+nDecember)
      January   -> (year-1,date+nJanuary)
      February  -> (year-1,date+nFebruary)

ydPairToDate :: YDPair -> Date
ydPairToDate (years,days) =
   if days<=nSeptember
   then
      if days<=nJune
      then
         if days<=nMay
         then
            if days<=nApril
            then -- March
               (years,March,days-nMarch)
            else -- April
               (years,April,days-nApril)
         else -- May
            (years,May,days-nMay)
      else
         if days<=nAugust
         then
            if days<=nJuly
            then -- June
               (years,June,days-nJune)
            else -- July
               (years,July,days-nJuly)
         else -- August
            (years,August,days-nAugust)
   else   
      if days<=nDecember
      then
         if days<=nNovember
         then
            if days<=nOctober
            then -- September
               (years,September,days-nSeptember)
            else -- October
               (years,October,days-nOctober)
         else -- November
            (years,November,days-nNovember)
      else
         if days<=nFebruary
         then
            if days<=nJanuary
            then -- December
               (years,December,days-nDecember)
            else -- January
               (years+1,January,days-nJanuary)
         else -- February
            (years+1,February,days-nFebruary)
\end{code}
