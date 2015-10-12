-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module SimpleQueue
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of simple queues
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- sequence operations
    empty,single,cons,snoc,append,lview,lhead,ltail,rview,rhead,rtail,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldl,foldr1,foldl1,reducer,reducel,reduce1,
    copy,tabulate,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- documentation
    moduleName,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(Just2,Nothing2)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import EdisonPrelude(Maybe2(Just2,Nothing2))
import qualified Sequence as S ( Sequence(..) )
import SequenceDefaults
import qualified ListSeq as L
import Monad
import QuickCheck

-- signatures for exported functions
moduleName     :: String
empty          :: Seq a
single         :: a -> Seq a
cons           :: a -> Seq a -> Seq a
snoc           :: Seq a -> a -> Seq a
append         :: Seq a -> Seq a -> Seq a
lview          :: Seq a -> Maybe2 a (Seq a)
lhead          :: Seq a -> a
ltail          :: Seq a -> Seq a
rview          :: Seq a -> Maybe2 (Seq a) a
rhead          :: Seq a -> a
rtail          :: Seq a -> Seq a
null           :: Seq a -> Bool
size           :: Seq a -> Int
concat         :: Seq (Seq a) -> Seq a
reverse        :: Seq a -> Seq a
reverseOnto    :: Seq a -> Seq a -> Seq a
fromList       :: [a] -> Seq a
toList         :: Seq a -> [a]
map            :: (a -> b) -> Seq a -> Seq b
concatMap      :: (a -> Seq b) -> Seq a -> Seq b
foldr          :: (a -> b -> b) -> b -> Seq a -> b
foldl          :: (b -> a -> b) -> b -> Seq a -> b
foldr1         :: (a -> a -> a) -> Seq a -> a
foldl1         :: (a -> a -> a) -> Seq a -> a
reducer        :: (a -> a -> a) -> a -> Seq a -> a
reducel        :: (a -> a -> a) -> a -> Seq a -> a
reduce1        :: (a -> a -> a) -> Seq a -> a
copy           :: Int -> a -> Seq a
tabulate       :: Int -> (Int -> a) -> Seq a
inBounds       :: Seq a -> Int -> Bool
lookup         :: Seq a -> Int -> a
lookupM        :: Seq a -> Int -> Maybe a
lookupWithDefault :: a -> Seq a -> Int -> a
update         :: Int -> a -> Seq a -> Seq a
adjust         :: (a -> a) -> Int -> Seq a -> Seq a
mapWithIndex   :: (Int -> a -> b) -> Seq a -> Seq b
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
take           :: Int -> Seq a -> Seq a
drop           :: Int -> Seq a -> Seq a
splitAt        :: Int -> Seq a -> (Seq a, Seq a)
subseq         :: Int -> Int -> Seq a -> Seq a
filter         :: (a -> Bool) -> Seq a -> Seq a
partition      :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
takeWhile      :: (a -> Bool) -> Seq a -> Seq a
dropWhile      :: (a -> Bool) -> Seq a -> Seq a
splitWhile     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
zip            :: Seq a -> Seq b -> Seq (a,b)
zip3           :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zipWith        :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith3       :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
unzip          :: Seq (a,b) -> (Seq a, Seq b)
unzip3         :: Seq (a,b,c) -> (Seq a, Seq b, Seq c)
unzipWith      :: (a -> b) -> (a -> c) -> Seq a -> (Seq b, Seq c)
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> Seq a -> (Seq b, Seq c, Seq d)

moduleName = "SimpleQueue"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 5.2.
-- and
--   F. Warren Burton. "An efficient functional implementation of FIFO queues".
--   Information Processing Letters, 14(5):205-206, July 1982.

data Seq a = Q [a] [a]
  -- invariant: front empty only if rear also empty

-- not exported
makeQ [] ys = Q (L.reverse ys) []
makeQ xs ys = Q xs ys

empty = Q [] []
single x = Q [x] []
cons x (Q xs ys) = Q (x:xs) ys

snoc (Q [] _) y = Q [y] []
snoc (Q xs ys) y = Q xs (y:ys)

append (Q xs1 ys1) (Q xs2 ys2) =
    Q (xs1 ++ L.reverseOnto ys1 xs2) ys2

lview (Q [] _) = Nothing2
lview (Q [x] ys) = Just2 x (Q (L.reverse ys) [])
lview (Q (x:xs) ys) = Just2 x (Q xs ys)

lhead (Q [] _) = error "SimpleQueue.lhead: empty sequence"
lhead (Q (x:xs) _) = x

ltail (Q [x] ys) = Q (L.reverse ys) []
ltail (Q (x:xs) ys) = Q xs ys
ltail q@(Q [] _) = q

rview (Q xs (y:ys)) = Just2 (Q xs ys) y
rview (Q xs []) =
  case L.rview xs of
    Nothing2 -> Nothing2
    Just2 xs' x -> Just2 (Q xs' []) x

rhead (Q xs (y:ys)) = y
rhead (Q [] []) = error "SimpleQueue.rhead: empty sequence"
rhead (Q xs []) = L.rhead xs

rtail (Q xs (y:ys)) = Q xs ys
rtail q@(Q [] []) = q
rtail (Q xs []) = Q (L.rtail xs) []

null (Q [] _) = True
null _ = False

size (Q xs ys) = length xs + length ys

reverse (Q xs []) = Q (L.reverse xs) []
reverse (Q xs ys) = Q ys xs

reverseOnto (Q xs1 ys1) (Q xs2 ys2) =
    Q (ys1 ++ L.reverseOnto xs1 xs2) ys2

fromList xs = Q xs []

toList (Q xs []) = xs
toList (Q xs ys) = xs ++ L.reverse ys

map f (Q xs ys) = Q (L.map f xs) (L.map f ys)

-- local fn on lists
revfoldr f e [] = e
revfoldr f e (x:xs) = revfoldr f (f x e) xs

-- local fn on lists
revfoldl f e [] = e
revfoldl f e (x:xs) = f (revfoldl f e xs) x

foldr f e (Q xs ys) = L.foldr f (revfoldr f e ys) xs

foldl f e (Q xs ys) = revfoldl f (L.foldl f e xs) ys

foldr1 f (Q xs (y:ys)) = L.foldr f (revfoldr f y ys) xs
foldr1 f (Q [] []) = error "SimpleQueue.foldr1: empty sequence"
foldr1 f (Q xs []) = L.foldr1 f xs

foldl1 f (Q (x:xs) ys) = revfoldl f (L.foldl f x xs) ys
foldl1 f (Q [] _) = error "SimpleQueue.foldl1: empty sequence"

filter p (Q xs ys) = makeQ (L.filter p xs) (L.filter p ys)

partition p (Q xs ys)
  = (makeQ xsT ysT, makeQ xsF ysF)
 where
   (xsT,xsF) = L.partition p xs
   (ysT,ysF) = L.partition p ys

-- the remaining functions all use defaults

concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
copy = copyUsingLists
tabulate = tabulateUsingLists
inBounds = inBoundsUsingLookupM
lookup = lookupUsingLookupM
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingLookupM
update = updateUsingAdjust
adjust = adjustUsingLists
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
take = takeUsingLists
drop = dropUsingLists
splitAt = splitAtDefault
subseq = subseqDefault
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
unzip = unzipUsingLists
unzip3 = unzip3UsingLists
unzipWith = unzipWithUsingLists
unzipWith3 = unzipWith3UsingLists

-- instances

instance S.Sequence Seq where
  {empty = empty; single = single; cons = cons; snoc = snoc;
   append = append; lview = lview; lhead = lhead; ltail = ltail;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   map = map; concatMap = concatMap; foldr = foldr; foldl = foldl;
   foldr1 = foldr1; foldl1 = foldl1; reducer = reducer; 
   reducel = reducel; reduce1 = reduce1; copy = copy; 
   tabulate = tabulate; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust;
   mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldlWithIndex = foldlWithIndex;
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = single
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  q1 == q2 = toList q1 == toList q2

instance Show a => Show (Seq a) where
  show q = show (toList q)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 ys <- arbitrary
                 return (if L.null xs then Q ys [] else Q xs ys)

  coarbitrary (Q xs ys) = coarbitrary xs . coarbitrary ys
