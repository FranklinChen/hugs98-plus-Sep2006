--
-- Time primitives for Hugs98.
--
module Hugs.Time
	( getClockTimePrim
	, toCalTimePrim
	, toClockTimePrim

	, getCPUTime		-- :: IO Integer
	, clockTicks		-- :: Int
	) where

primitive getClockTimePrim :: IO (Int,Int)
primitive toCalTimePrim ::
	Int -> Int -> IO (Int,Int,Int,Int,Int,Int,Int,Int,Int,String,Int)
primitive toClockTimePrim ::
	Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO Int

picoSec :: Integer
picoSec = 1000000000000 -- 10^12

getCPUTime :: IO Integer
getCPUTime = do
   (usec, unsec, ssec, snsec) <- getCPUUsage
   return (picoSec * fromIntegral usec  +
   	   1000    * fromIntegral unsec + 
	   picoSec * fromIntegral ssec  + 
	   1000    * fromIntegral snsec)
	   
primitive getCPUUsage  :: IO (Int,Int,Int,Int)
primitive clockTicks   :: Int
