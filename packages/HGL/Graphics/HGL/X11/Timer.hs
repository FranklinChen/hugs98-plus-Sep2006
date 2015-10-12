-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.X11.Timer
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.X11.Timer
	( Timer, new, stop
        , Timers, newTimers, clearTimers, nextTick, fireTimers
	) where

import Control.Concurrent
	( MVar, newMVar, takeMVar, putMVar, readMVar )
import Graphics.HGL.Internals.Utilities( modMVar_ )
import Graphics.HGL.Internals.Types

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

data Timer = Timer
  { period :: Time           -- how often does it fire
  , action :: IO ()          -- what to do when it does
  , tag    :: MVar ()        -- something that supports an equality test
  }

-- A standard timer implementation using a list of (delta-time,timer) pairs.
type Timers = MVar [(Time, Timer)]

newTimers   :: IO Timers
clearTimers :: Timers -> IO ()
nextTick    :: Timers -> IO (Maybe Time)
fireTimers  :: Timers -> Time -> IO ()

new  :: Timers -> Time -> IO () -> IO Timer
stop :: Timers -> Timer -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

newTimers = do
  newMVar []

-- This will only work if the mvar is non-empty.
-- Fortunately, all operations on timers do atomic updates (modMVar)
-- so this should be true.
clearTimers ts = do
  modMVar_ ts (const [])

fireTimers timers t = do
  xs <- takeMVar timers
  let (ts,xs') = firedTimers t xs
      xs''     = foldr insert xs' ts
  putMVar timers xs''
  mapM_ action ts
 where
  insert :: Timer -> [(Time,Timer)] -> [(Time,Timer)]
  insert timer = insertTimer (period timer) timer

nextTick timers = do
  ts <- readMVar timers
  case ts of
    ((t,_):_) -> return (Just t)
    _         -> return Nothing

new timers t a = do
  tag <- newMVar ()
  let timer = Timer{period=t, action=a, tag=tag}
  modMVar_ timers (insertTimer t timer) 
  return timer

stop timers timer = do
  modMVar_ timers (deleteTimer timer) 

instance Eq Timer where
  t1 == t2 = tag t1 == tag t2

insertTimer :: Time -> Timer -> [(Time,Timer)] -> [(Time,Timer)]
insertTimer t timer []     = [(t,timer)]
insertTimer t timer (x@(t',timer'):xs)
  | t <= t'
  = (t,timer) : (t'-t, timer') : xs
  | otherwise
  = x : insertTimer (t-t') timer xs

deleteTimer :: Timer -> [(Time,Timer)] -> [(Time,Timer)]
deleteTimer timer [] = []
deleteTimer timer (x@(t',timer'):xs)
  | timer == timer'
  = case xs of
      []                 -> []
      (t'', timer''):xs' -> (t'+t'', timer''):xs'
  | otherwise 
  = x : deleteTimer timer xs

-- we could try to avoid timer drift by returning how "late" we are
-- in firing the timer
-- Maybe a better approach is to make use of the real-time clock provided
-- by the OS and stay in sync with that?
firedTimers :: Time -> [(Time,Timer)] -> ([Timer],[(Time,Timer)])
firedTimers t [] = ([],[])
firedTimers t ((t',timer):xs)
  | t < t'
  = ([], (t'-t,timer):xs)
  | otherwise
  = let (timers, xs') = firedTimers (t-t') xs
    in (timer : timers, xs')

----------------------------------------------------------------
-- End
----------------------------------------------------------------
