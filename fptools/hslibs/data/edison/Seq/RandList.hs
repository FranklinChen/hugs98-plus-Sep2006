-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module RandList
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type
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
import qualified Sequence as S( Sequence(..) )
import SequenceDefaults
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

moduleName = "RandList"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 9.3.1.
-- and
--   Chris Okasaki. "Purely Functional Random Access Lists".  FPCA'95,
--   pages 86-95.

data Tree a = L a | T a (Tree a) (Tree a)   deriving (Eq)
data Seq a = E | C !Int (Tree a) (Seq a)    --deriving (Eq)
  -- want to derive Eq but can't because of GHC bug

half :: Int -> Int
half n = n `quot` 2  -- use a shift?

empty = E
single x = C 1 (L x) E

cons x xs@(C i s (C j t xs'))
    | i == j = C (1 + i + j) (T x s t) xs'
cons x xs = C 1 (L x) xs

copy n x = if n <= 0 then E else buildTrees (1::Int) (L x)
  where buildTrees j t
          | j > n     = takeTrees n (half j) (child t) E
          | otherwise = buildTrees (1 + j + j) (T x t t)

        takeTrees i j t xs
          | i >= j = takeTrees (i - j) j t (C j t xs)
          | i > 0  = takeTrees i (half j) (child t) xs
          | otherwise = xs

        child (T x s t) = t

lview E = Nothing2
lview (C _ (L x) xs) = Just2 x xs
lview (C i (T x s t) xs) = Just2 x (C j s (C j t xs))
  where j = half i

lhead E = error "RandList.lhead: empty sequence"
lhead (C _ (L x) xs) = x
lhead (C _ (T x s t) xs) = x

ltail E = E
ltail (C _ (L x) xs) = xs
ltail (C i (T x s t) xs) = C j s (C j t xs)
  where j = half i

rhead E = error "RandList.rhead: empty sequence"
rhead (C _ t E) = treeLast t
  where treeLast (L x) = x
        treeLast (T x s t) = treeLast t
rhead (C _ t xs) = rhead xs

null E = True
null _ = False

size xs = sz xs
  where sz E = (0::Int)
        sz (C j t xs) = j + sz xs

reverseOnto E ys = ys
reverseOnto (C _ t xs) ys = reverseOnto xs (revTree t ys)
  where revTree (L x) ys = cons x ys
        revTree (T x s t) ys = revTree t (revTree s (cons x ys))

map f E = E
map f (C j t xs) = C j (mapTree f t) (map f xs)
  where mapTree f (L x) = L (f x)
        mapTree f (T x s t) = T (f x) (mapTree f s) (mapTree f t)

foldr f e E = e
foldr f e (C _ t xs) = foldTree t (foldr f e xs)
  where foldTree (L x) e = f x e
        foldTree (T x s t) e = f x (foldTree s (foldTree t e))

foldl f e E = e
foldl f e (C _ t xs) = foldl f (foldTree e t) xs
  where foldTree e (L x) = f e x
        foldTree e (T x s t) = foldTree (foldTree (f e x) s) t

reduce1 f xs = case lview xs of
                 Nothing2 -> error "RandList.reduce1: empty seq"
                 Just2 x xs -> red1 x xs
  where red1 x E = x
        red1 x (C j t xs) = red1 (redTree x t) xs

        redTree x (L y) = f x y
        redTree x (T y s t) = redTree (redTree (f x y) s) t

inBounds xs i = inb xs i
  where inb E i = False
        inb (C j t xs) i
          | i < j     = (i >= 0)
          | otherwise = inb xs (i - j)

lookup xs i = look xs i
  where look E i = error "RandList.lookup: bad subscript"
        look (C j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (L x) i
            | i == 0    = x
            | otherwise = error "RandList.lookup: bad subscript"
        lookTree j (T x s t) i
            | i > k   = lookTree k t (i - 1 - k)
            | i /= 0  = lookTree k s (i - 1)
            | otherwise = x
          where k = half j

lookupM xs i = look xs i
  where look E i = Nothing
        look (C j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (L x) i
            | i == 0    = Just x
            | otherwise = Nothing
        lookTree j (T x s t) i
            | i > k  = lookTree k t (i - 1 - k)
            | i /= 0 = lookTree k s (i - 1)
            | otherwise = Just x
          where k = half j

lookupWithDefault d xs i = look xs i
  where look E i = d
        look (C j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (L x) i
            | i == 0    = x
            | otherwise = d
        lookTree j (T x s t) i
            | i > k   = lookTree k t (i - 1 - k)
            | i /= 0  = lookTree k s (i - 1)
            | otherwise = x
          where k = half j

update i y xs = upd i xs
  where upd i E = E
        upd i (C j t xs)
            | i < j     = C j (updTree i j t) xs
            | otherwise = C j t (upd (i - j) xs)

        updTree i j t@(L x)
            | i == 0    = L y
            | otherwise = t
        updTree i j (T x s t)
            | i > k   = T x s (updTree (i - 1 - k) k t)
            | i /= 0  = T x (updTree (i - 1) k s) t
            | otherwise = T y s t
          where k = half j

adjust f i xs = adj i xs
  where adj i E = E
        adj i (C j t xs)
            | i < j     = C j (adjTree i j t) xs
            | otherwise = C j t (adj (i - j) xs)

        adjTree i j t@(L x)
            | i == 0    = L (f x)
            | otherwise = t
        adjTree i j (T x s t)
            | i > k  = T x s (adjTree (i - 1 - k) k t)
            | i /= 0 = T x (adjTree (i - 1) k s) t
            | otherwise = T (f x) s t
          where k = half j

drop n xs = if n < 0 then xs else drp n xs
  where drp i E = E
        drp i (C j t xs)
            | i < j     = drpTree i j t xs
            | otherwise = drp (i - j) xs

        drpTree 0 j t xs = C j t xs
        drpTree i j (L x) xs = error "RandList.drop: bug.  Impossible case!"
        drpTree i j (T x s t) xs
            | i > k     = drpTree (i - 1 - k) k t xs
            | otherwise = drpTree (i - 1) k s (C k t xs)
          where k = half j

-- the remaining functions all use defaults

snoc = snocUsingFoldr
append = appendUsingFoldr
rview = rviewDefault
rtail = rtailUsingLview
concat = concatUsingFoldr
reverse = reverseUsingReverseOnto
fromList = fromListUsingCons
toList = toListUsingFoldr
concatMap = concatMapUsingFoldr
foldr1 = foldr1UsingLview
foldl1 = foldl1UsingFoldl
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
tabulate = tabulateUsingLists
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
take = takeUsingLists
splitAt = splitAtDefault
filter = filterUsingFoldr
partition = partitionUsingFoldr
subseq = subseqDefault
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview

-- for zips, could optimize by calculating which one is shorter and
-- retaining its shape

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

-- want to derive the following instance but can't because of GHC bug
instance Eq a => Eq (Seq a) where
  C i tx xs == C j ty ys = (i == j) && (tx == ty) && (xs == ys)
  E == E = True
  _ == _ = False

instance Show a => Show (Seq a) where
  show xs = show (toList xs)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 return (fromList xs)

  coarbitrary xs = coarbitrary (toList xs)
