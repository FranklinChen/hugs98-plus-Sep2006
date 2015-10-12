-----------------------------------------------------------------------------
-- Lazy State Thread module
-- 
-- This library provides support for both lazy and strict state threads,
-- as described in the PLDI '94 paper by John Launchbury and Simon Peyton
-- Jones.  In addition to the monad ST, it also provides mutable variables
-- STRef and mutable arrays STArray.  It is identical to the ST module
-- except that the ST instance is lazy.
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Hugs.LazyST 
	( ST
	, runST
	, unsafeInterleaveST
	, fixST 

	, lazyToStrictST
	, strictToLazyST
	) where

import qualified Hugs.ST as ST
import Control.Monad   

-----------------------------------------------------------------------------

newtype ST s a = ST (State s -> (a, State s))

unST :: ST s a -> State s -> (a, State s)
unST (ST f) = f

runST :: (forall s. ST s a) -> a
runST m = fst (unST m S)

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST m) = return (fst (m S))

fixST :: (a -> ST s a) -> ST s a
fixST f = ST (\s -> let (x,s') = unST (f x) s in (x,s'))

instance Functor (ST s) where
    fmap = liftM

instance Monad (ST s) where
    return a = ST (\s -> (a, s))
    ST m >>= f = ST (\S -> let (a,s') = m S in unST (f a) s')
    -- ST m >>= f = ST (\s -> let (a,s') = m s in unST (f a) s')

-----------------------------------------------------------------------------

data State s = S

-----------------------------------------------------------------------------

lazyToStrictST :: ST s a -> ST.ST s a
lazyToStrictST (ST m) = ST.ST (\k -> case m S of (a,S) -> k a)

strictToLazyST :: ST.ST s a -> ST s a
strictToLazyST (ST.ST m) = ST (\S -> m delay)
--	\s -> let (a',s') = case s of S -> m (\a -> (a,S)) in (a',s'))

delay :: a -> (a, State s)
delay a = (a,S)

-----------------------------------------------------------------------------
