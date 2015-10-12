{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.Time.Calendar
import Control.Monad

days ::[Day]
days =
	[
	fromGregorian 2005 2 28,
	fromGregorian 2004 2 29,
	fromGregorian 2004 1 31,
	fromGregorian 2004 12 31,
	fromGregorian 2005 7 1,
	fromGregorian 2005 4 21,
	fromGregorian 2005 6 30
	]

increments :: [Integer]
increments = [-10,-4,-1,0,1,7,83]

adders :: [(String,Integer -> Day -> Day)]
adders =
	[
	("day",addDays),
	("month (clip)",addGregorianMonthsClip),
	("month (roll over)",addGregorianMonthsRollOver),
	("year (clip)",addGregorianYearsClip),
	("year (roll over)",addGregorianYearsRollOver)
	]

resultDays :: [String]
resultDays = do
	(aname,adder) <- adders
	increment <- increments
	day <- days
	return ((showGregorian day) ++ " + " ++ (show increment) ++ " * " ++ aname ++ " = " ++ showGregorian (adder increment day))

main :: IO ()
main = do
	mapM_ putStrLn resultDays
