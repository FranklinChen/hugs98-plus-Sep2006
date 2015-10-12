{-----------------------------------------------------------------------------

                   A LIBRARY OF MEMOIZATION COMBINATORS

                            15th September 1999

	                         Byron Cook
			            OGI

This Hugs module implements several flavors of memoization functions,
as described in Haskell Workshop 1997.
-----------------------------------------------------------------------------}

module Hugs.Memo(
        memo,  
        memoN,  
        memoFix,
        memoFixN,
        cache, 
        cacheN, 
        cacheFix,
        cacheFixN
        ) where

import Hugs.ST
-- import Hugs.IOExts (unsafePtrEq)
-- import Debug.Trace (trace)

memo      :: (a -> b) -> (a -> b)
memoN     :: Int -> (a -> b) -> (a -> b)
memoFix   :: ((a -> b) -> (a -> b)) -> (a -> b)
memoFixN  :: Int -> ((a -> b) -> (a -> b)) -> (a -> b)
cache     :: (a -> b) -> (a -> b)
cacheN    :: Int -> (a -> b) -> (a -> b)
cacheFix  :: ((a -> b) -> (a -> b)) -> (a -> b)
cacheFixN :: Int -> ((a -> b) -> (a -> b)) -> (a -> b)

----------------------------------------------------------------
-- Memoization Functions (memo-tables are hash-tables)
----------------------------------------------------------------
memo          = memoN defaultSize 
memoN         = mkMemo eql hash 

memoFix       = memoFixN defaultSize 
memoFixN n f  = let g = f h
                    h = memoN n g
                in g

----------------------------------------------------------------
-- Caching Functions (memo-tables are caches)
----------------------------------------------------------------
cache          = cacheN defaultSize
cacheN         = mkCache eql hash
cacheFix       = cacheFixN defaultSize
cacheFixN n f  = let g = f h
                     h = cacheN n g
                 in g

----------------------------------------------------------------
-- Type synonyms
----------------------------------------------------------------
type TaintedEq a   = a -> a -> ST Mem Bool
type HashTable a b = STArray Mem Int [(a,b)]
type Cache a b     = STArray Mem Int (Maybe (a,b))
type HashSize      = Int
type HashFunc a    = a -> ST Mem Int
type Mem           = ()


----------------------------------------------------------------
-- Foundation functions
----------------------------------------------------------------
defaultSize :: HashSize
defaultSize = 40

memoize :: ST Mem t -> (t -> a -> b -> ST Mem b) -> 
           (a -> b) -> a -> b
memoize new access f = {-trace "memoize" $-} unsafeRunST $ do 
  t <- new
  return (\x -> unsafeRunST $ access t x (f x))


mkMemo  :: TaintedEq a -> HashFunc a -> Int -> (a -> c) -> (a -> c)
mkCache :: TaintedEq a -> HashFunc a -> Int -> (a -> c) -> (a -> c)

mkCache e h sz = memoize (newCache sz) (accessCache e h sz)
mkMemo  e h sz = memoize (newHash sz)  (accessHash e  h sz)


----------------------------------------------------------------
-- Hash and Cache Tables
----------------------------------------------------------------
accessHash  :: TaintedEq a ->  
               HashFunc a -> 
               Int -> 
               HashTable a b -> 
               a -> b -> ST Mem b

accessHash equal h sz table x v = do 
  hv' <- h x
  let hv = hv' `mod` sz
  l <- readSTArray table hv
  find l l hv
 where find l [] hv = {-trace "miss " $-} do
         u <- writeSTArray table  hv ((x,v):l) 
         case u of {() -> return v}
       find l ((x',v'):xs) hv = do
         a <- equal x x'
         if a then {-trace "hit "-} (return $ v')
          else find l xs hv

newHash :: Int -> ST Mem (HashTable a b)
newHash n = newSTArray (0,n) []


accessCache  :: TaintedEq a ->
                HashFunc a ->
                Int ->
                Cache a b ->
                a -> b -> ST Mem b

accessCache equal h sz table x v = do 
  hv' <- h x 
  let hv = hv' `mod` sz 
  l <-  readSTArray table hv
  case l of
     Nothing      -> do u <- writeSTArray table hv (Just (x,v))
                        case u of {() -> return v}
     Just (x',y)  -> do e <- equal x' x
                        if e then return y
                         else do u <- writeSTArray table hv (Just (x,v))
                                 case u of {() -> return v}

newCache :: Int -> ST Mem (Cache a b)
newCache n = newSTArray (0,n) Nothing

------------------------------------------------------------------
-- These functions are bad --- dont pay attention to them

-- lisp style eql --- as described in "Lazy-memo functions"
primitive eql "IOEql" :: a -> a -> ST Mem Bool
-- a `eql` b = return (a `unsafePtrEq` b)

-- hash based on addresses (or values if the arg is a base type)
primitive hash "IOHash" :: a -> ST Mem Int

------------------------------------------------------------------
