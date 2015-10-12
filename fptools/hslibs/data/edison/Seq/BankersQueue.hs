-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module BankersQueue
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of banker's queues
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

moduleName = "BankersQueue"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 6.3.2.
-- and
--   Chris Okasaki. "Simple and Efficient Purely Functional Queues and Deques".
--   Journal of Functional Programming, 5(4):583-592, October 1995.

data Seq a = Q !Int [a] [a] !Int
  -- invariant: front at least as long as rear

-- not exported
makeQ i xs ys j
  | j > i = Q (i + j) (xs ++ L.reverse ys) [] 0
  | otherwise = Q i xs ys j

-- not exported
inc i = i + 1

-- not exported
dec i = i - 1

empty = Q 0 [] [] 0
single x = Q 1 [x] [] 0
cons x (Q i xs ys j) = Q (inc i) (x:xs) ys j
snoc (Q i xs ys j) y = makeQ i xs (y:ys) (inc j)

append (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (xs1 ++ L.reverseOnto ys1 xs2) ys2 j2

lview (Q _ [] _ _) = Nothing2
lview (Q i (x:xs) ys j) = Just2 x (makeQ (dec i) xs ys j)

lhead (Q _ [] _ _) = error "BankersQueue.lhead: empty sequence"
lhead (Q _ (x:xs) _ _) = x

ltail (Q i (x:xs) ys j) = makeQ (dec i) xs ys j
ltail q = q -- empty case

rview (Q i xs (y:ys) j) = Just2 (Q i xs ys (dec j)) y
rview (Q i xs [] _) =
  case L.rview xs of
    Nothing2 -> Nothing2
    Just2 xs' x -> Just2 (Q (dec i) xs' [] 0) x

rhead (Q i xs (y:ys) j) = y
rhead (Q _ [] [] _) = error "BankersQueue.rhead: empty sequence"
rhead (Q i xs [] _) = L.rhead xs

rtail (Q i xs (y:ys) j) = Q i xs ys (dec j)
rtail q@(Q _ [] [] _) = q -- empty case
rtail (Q i xs [] _) = Q (dec i) (L.rtail xs) [] 0

null (Q i _ _ _) = (i == 0)
size (Q i xs ys j) = i + j
reverse (Q i xs ys j) = makeQ j ys xs i

reverseOnto (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (ys1 ++ L.reverseOnto xs1 xs2) ys2 j2

fromList xs = Q (length xs) xs [] 0

toList (Q i xs ys j)
  | j == 0 = xs
  | otherwise = xs ++ L.reverse ys

map f (Q i xs ys j) = Q i (L.map f xs) (L.map f ys) j

-- local fn on lists
revfoldr f e [] = e
revfoldr f e (x:xs) = revfoldr f (f x e) xs

-- local fn on lists
revfoldl f e [] = e
revfoldl f e (x:xs) = f (revfoldl f e xs) x

foldr f e (Q i xs ys j) = L.foldr f (revfoldr f e ys) xs
foldl f e (Q i xs ys j) = revfoldl f (L.foldl f e xs) ys

foldr1 f (Q i xs (y:ys) j) = L.foldr f (revfoldr f y ys) xs
foldr1 f (Q i xs [] _)
  | i == 0 = error "BankersQueue.foldr1: empty sequence"
  | otherwise = L.foldr1 f xs

foldl1 f (Q i (x:xs) ys j) = revfoldl f (L.foldl f x xs) ys
foldl1 f _ = error "BankersQueue.foldl1: empty sequence"

copy n x 
  | n < 0     = empty
  | otherwise = Q n (L.copy n x) [] 0
tabulate n f 
  | n < 0     = empty
  | otherwise = Q n (L.tabulate n f) [] 0

-- reduce1: given sizes could do more effective job of dividing evenly!

lookup (Q i xs ys j) idx
  | idx < i   = L.lookup xs idx
  | otherwise = L.lookup ys (j - (idx - i) - 1)

lookupM (Q i xs ys j) idx
  | idx < i   = L.lookupM xs idx
  | otherwise = L.lookupM ys (j - (idx - i) - 1)

lookupWithDefault d (Q i xs ys j) idx
  | idx < i   = L.lookupWithDefault d xs idx
  | otherwise = L.lookupWithDefault d ys (j - (idx - i) - 1)

update idx e q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.update idx e xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.update k' e ys) j

adjust f idx q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.adjust f idx xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.adjust f k' ys) j

{- 
could do
  mapWithIndex   :: (Int -> a -> b) -> s a -> s b
  foldrWithIndex :: (Int -> a -> b -> b) -> b -> s a -> b
  foldlWithIndex :: (b -> Int -> a -> b) -> b -> s a -> b
but don't bother for now
-}

take len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then empty
    else Q len (L.take len xs) [] 0
  else let len' = len - i in
    if len' >= j then q
    else Q i xs (L.drop (j - len') ys) len'

drop len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then q
    else makeQ (i - len) (L.drop len xs) ys j
  else let len' = len - i in
    if len' >= j then empty
    else Q (j - len') (L.reverse (L.take (j - len') ys)) [] 0
  -- could write more efficient version of reverse (take ...) 

splitAt idx q@(Q i xs ys j) =
  if idx <= i then
    if idx <= 0 then (empty, q)
    else let (xs',xs'') = L.splitAt idx xs
         in (Q idx xs' [] 0, makeQ (i - idx) xs'' ys j)
  else let idx' = idx - i in
    if idx' >= j then (q, empty)
    else let (ys', ys'') = L.splitAt (j - idx') ys
         in (Q i xs ys'' idx', Q (j - idx') (L.reverse ys') [] 0)
      -- could do splitAt followed by reverse more efficiently...
  

-- the remaining functions all use defaults

concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
inBounds = inBoundsUsingSize
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
subseq = subseqDefault
filter = filterUsingLists
partition = partitionUsingLists
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
   update = update; adjust = adjust; mapWithIndex = mapWithIndex;
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
  q1 == q2 =
    (size q1 == size q2) && (toList q1 == toList q2)

instance Show a => Show (Seq a) where
  show q = show (toList q)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary =
    do xs <- arbitrary
       ys <- arbitrary
       return (let i = L.size xs
                   j = L.size ys
               in if i >= j then Q i xs ys j else Q j ys xs i)

  coarbitrary (Q i xs ys j) = coarbitrary xs . coarbitrary ys
