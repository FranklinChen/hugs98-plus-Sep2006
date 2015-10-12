-- This is a modification of the calendar program described in section 4.5
-- of Bird and Wadler's ``Introduction to functional programming'', with
-- two ways of printing the calendar ... as in B+W, or like UNIX `cal':

-- Run using:  calFor "1996"
--        or:  putStr (calendar 1996)
--        or:  putStr (cal 1996)

module Calendar( calendar, cal, calFor, calProg ) where
import Gofer
import List(zip4)
import IO(hPutStr,stderr)
import System( getArgs, getProgName, exitWith, ExitCode(..) )
import Char (digitToInt, isDigit)

-- Picture handling:

infixr 5 `above`, `beside`

type Picture   =  [[Char]]

height, width :: Picture -> Int
height p       = length p
width  p       = length (head p)

above, beside :: Picture -> Picture -> Picture
above          = (++)
beside         = zipWith (++)

stack, spread :: [Picture] -> Picture
stack          = foldr1 above
spread         = foldr1 beside

empty         :: (Int,Int) -> Picture
empty (h,w)    = replicate h (replicate w ' ')

block, blockT :: Int -> [Picture] -> Picture
block n        = stack . map spread . groupsOf n
blockT n       = spread . map stack . groupsOf n

groupsOf      :: Int -> [a] -> [[a]]
groupsOf n []  = []
groupsOf n xs  = take n xs : groupsOf n (drop n xs)

lframe        :: (Int,Int) -> Picture -> Picture
lframe (m,n) p = (p `beside` empty (h,n-w)) `above` empty (m-h,n)
		 where h = height p
                       w = width p

-- Information about the months in a year:

monthLengths year = [31,feb,31,30,31,30,31,31,30,31,30,31]
                    where feb | leap year = 29
                              | otherwise = 28

leap year         = if year`mod`100 == 0 then year`mod`400 == 0
                                         else year`mod`4   == 0

monthNames        = ["January","February","March","April",
		     "May","June","July","August",
		     "September","October","November","December"]

jan1st year       = (year + last`div`4 - last`div`100 + last`div`400) `mod` 7
                    where last = year - 1

firstDays year    = take 12
                         (map (`mod`7)
                              (scanl (+) (jan1st year) (monthLengths year)))

-- Producing the information necessary for one month:

dates fd ml = map (date ml) [1-fd..42-fd]
              where date ml d | d<1 || ml<d  = ["   "]
                              | otherwise    = [rjustify 3 (show d)]

-- The original B+W calendar:

calendar :: Int -> String
calendar  = unlines . block 3 . map picture . months
            where picture (mn,yr,fd,ml)  = title mn yr `above` table fd ml
                  title mn yr    = lframe (2,25) [mn ++ " " ++ show yr]
                  table fd ml    = lframe (8,25)
                                          (daynames `beside` entries fd ml)
                  daynames       = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]
                  entries fd ml  = blockT 7 (dates fd ml)
                  months year    = zip4 monthNames
                                        (replicate 12 year)
                                        (firstDays year)
                                        (monthLengths year)

-- In a format somewhat closer to UNIX cal:

cal year = unlines (banner year `above` body year)
           where banner yr      = [cjustify 75 (show yr)] `above` empty (1,75)
                 body           = block 3 . map (pad . pic) . months
                 pic (mn,fd,ml) = title mn `above` table fd ml
                 pad p          = (side`beside`p`beside`side)`above`end
                 side           = empty (8,2)
                 end            = empty (1,25)
                 title mn       = [cjustify 21 mn]
                 table fd ml    = daynames `above` entries fd ml
                 daynames       = [" Su Mo Tu We Th Fr Sa"]
                 entries fd ml  = block 7 (dates fd ml)
                 months year    = zip3 monthNames
                                       (firstDays year)
                                       (monthLengths year)

-- For a standalone calendar program:
--
-- To use this with "runhugs" on Unix:
--
--   cat >cal
--   #! /usr/local/bin/runhugs
--   
--   > module Main( main ) where
--   > import Calendar
--   > main = calProg
--   <ctrl-D>
--
--   chmod 755 cal
--
--   ./cal 1997

calProg = do
         args <- getArgs
         case args of 
          [year] -> calFor year
          _      -> do
                      putStr "Usage: "
                      getProgName >>= putStr
                      putStrLn " year" 
                      exitWith (ExitFailure 1)

calFor year | illFormed = hPutStr stderr "Bad argument" >>
                          exitWith (ExitFailure 1)
            | otherwise = putStr (cal yr)
              where illFormed = null ds || not (null rs)
                    (ds,rs)   = span isDigit year
                    yr        = atoi ds
                    atoi s    = foldl (\a d -> 10*a+d) 0 (map digitToInt s)
       

-- End of calendar program
