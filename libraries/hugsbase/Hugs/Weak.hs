-- A first cut at implementing the (key,value) form of Weak pointers.
--
-- Notes (please refer to the draft specification for background):
--
--  - Programmers using weak pointers should call runFinalizer at
--    regular intervals to ensure that finalizers are scheduled for
--    execution.  This implementation provides functions runFinalizer,
--    finalizerWaiting, and runAllFinalizers to provide programmers with
--    control over the execution of finalizers.  None of these functions
--    are part of the current specification.
--
-- Tested with Hugs 98.

module Hugs.Weak(Weak,
	    mkWeak, deRefWeak, finalize, replaceFinalizer,
	    runFinalizer, finalizerWaiting, runAllFinalizers ) where

data Weak a

primitive mkWeak    :: k -> v -> Maybe (IO ()) -> IO (Weak v)
primitive deRefWeak :: Weak v -> IO (Maybe v)
primitive replaceFinalizer :: Weak v -> Maybe (IO ()) -> IO (Maybe (IO ()))
primitive finalize  :: Weak v -> IO ()
primitive weakPtrEq :: Weak a -> Weak a -> Bool

instance Eq (Weak a) where
  (==) = weakPtrEq

primitive runFinalizer     :: IO ()
primitive finalizerWaiting :: IO Bool

runAllFinalizers    :: IO ()
runAllFinalizers     = do waiting <- finalizerWaiting
			  if waiting then do runFinalizer
					     runAllFinalizers
				     else return ()

{- for testing purposes
primitive gc "primGC" :: IO ()

-- not a CAF!
test z = do
  { let k = [z]		-- use a list so we're sure it's heap allocated
  ; print k		-- this makes sure x is in whnf
  ; w <- mkWeak k "value" (Just (putStrLn ("Finalizer for "++show k)))
			-- note that the finalizer uses the key, but
			-- this shouldn't keep the weak ptr alive!
  ; showWeakPtr w
  ; gc
  ; print k		-- this makes sure k is still alive after the GC
  ; showWeakPtr w	-- so it's probably still alive here
  ; gc
  ; showWeakPtr w	-- but ought to be dead by here
  }

showWeakPtr :: Show a => Weak a -> IO ()
showWeakPtr w = do
  { x <- deRefWeak w
  ; print x
  }

-}

-- End of module Weak
