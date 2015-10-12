-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module ListSeq
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type synonym
    Seq,

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
import qualified Prelude
import EdisonPrelude(Maybe2(Just2,Nothing2))
import qualified List(partition)
import qualified Sequence as S ( Sequence(..) ) 

-- signatures for exported functions
moduleName     :: String
empty          :: [a]
single         :: a -> [a]
cons           :: a -> [a] -> [a]
snoc           :: [a] -> a -> [a]
append         :: [a] -> [a] -> [a]
lview          :: [a] -> Maybe2 a ([a])
lhead          :: [a] -> a
ltail          :: [a] -> [a]
rview          :: [a] -> Maybe2 ([a]) a
rhead          :: [a] -> a
rtail          :: [a] -> [a]
null           :: [a] -> Bool
size           :: [a] -> Int
concat         :: [[a]] -> [a]
reverse        :: [a] -> [a]
reverseOnto    :: [a] -> [a] -> [a]
fromList       :: [a] -> [a]
toList         :: [a] -> [a]
map            :: (a -> b) -> [a] -> [b]
concatMap      :: (a -> [b]) -> [a] -> [b]
foldr          :: (a -> b -> b) -> b -> [a] -> b
foldl          :: (b -> a -> b) -> b -> [a] -> b
foldr1         :: (a -> a -> a) -> [a] -> a
foldl1         :: (a -> a -> a) -> [a] -> a
reducer        :: (a -> a -> a) -> a -> [a] -> a
reducel        :: (a -> a -> a) -> a -> [a] -> a
reduce1        :: (a -> a -> a) -> [a] -> a
copy           :: Int -> a -> [a]
tabulate       :: Int -> (Int -> a) -> [a]
inBounds       :: [a] -> Int -> Bool
lookup         :: [a] -> Int -> a
lookupM        :: [a] -> Int -> Maybe a
lookupWithDefault :: a -> [a] -> Int -> a
update         :: Int -> a -> [a] -> [a]
adjust         :: (a -> a) -> Int -> [a] -> [a]
mapWithIndex   :: (Int -> a -> b) -> [a] -> [b]
foldrWithIndex :: (Int -> a -> b -> b) -> b -> [a] -> b
foldlWithIndex :: (b -> Int -> a -> b) -> b -> [a] -> b
take           :: Int -> [a] -> [a]
drop           :: Int -> [a] -> [a]
splitAt        :: Int -> [a] -> ([a], [a])
subseq         :: Int -> Int -> [a] -> [a]
filter         :: (a -> Bool) -> [a] -> [a]
partition      :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile      :: (a -> Bool) -> [a] -> [a]
dropWhile      :: (a -> Bool) -> [a] -> [a]
splitWhile     :: (a -> Bool) -> [a] -> ([a], [a])
zip            :: [a] -> [b] -> [(a,b)]
zip3           :: [a] -> [b] -> [c] -> [(a,b,c)]
zipWith        :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3       :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
unzip          :: [(a,b)] -> ([a], [b])
unzip3         :: [(a,b,c)] -> ([a], [b], [c])
unzipWith      :: (a -> b) -> (a -> c) -> [a] -> ([b], [c])
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> [a] -> ([b], [c], [d])

moduleName = "ListSeq"

type Seq a = [a]

empty = []
single x = [x]
cons = (:)
snoc s x = s ++ [x]
append = (++)

lview [] = Nothing2
lview (x:xs) = Just2 x xs

lhead [] = error "ListSeq.lhead: empty sequence"
lhead (x:xs) = x

ltail [] = []
ltail (x:xs) = xs

rview [] = Nothing2
rview xs = Just2 (rtail xs) (rhead xs)

rhead [] = error "ListSeq.rhead: empty sequence"
rhead (x:xs) = rh x xs
  where rh y [] = y
        rh y (x:xs) = rh x xs

rtail [] = []
rtail (x:xs) = rt x xs
  where rt y [] = []
        rt y (x:xs) = y : rt x xs

null = Prelude.null
size = length
concat = foldr append empty
reverse = Prelude.reverse

reverseOnto [] ys = ys
reverseOnto (x:xs) ys = reverseOnto xs (x:ys)

fromList xs = xs
toList xs = xs
map = Prelude.map
concatMap = Prelude.concatMap
foldr = Prelude.foldr
foldl = Prelude.foldl

foldr1 f [] = error "ListSeq.foldr1: empty sequence"
foldr1 f (x:xs) = fr x xs
  where fr y [] = y
        fr y (x:xs) = f y (fr x xs)

foldl1 f [] = error "ListSeq.foldl1: empty sequence"
foldl1 f (x:xs) = foldl f x xs

reducer f e [] = e
reducer f e xs = f (reduce1 f xs) e

reducel f e [] = e
reducel f e xs = f e (reduce1 f xs)

reduce1 f [] = error "ListSeq.reduce1: empty sequence"
reduce1 f [x] = x
reduce1 f (x1 : x2 : xs) = reduce1 f (f x1 x2 : pairup xs)
  where pairup (x1 : x2 : xs) = f x1 x2 : pairup xs
        pairup xs = xs
  -- can be improved using a counter and bit ops!

copy n x | n <= 0 = []
         | otherwise = x : copy (n-1) x
  -- depends on n to be unboxed, should test this!

tabulate n f = tab 0
  where tab i | i >= n = []
              | otherwise = f i : tab (i+1)
  -- depends on i (and n?) being unboxed, should check this!

inBounds xs i
  | i >= 0    = not (null (drop i xs))
  | otherwise = False

lookup xs i
  | i < 0 = error "ListSeq.lookup: bad subscript"
  | otherwise = case drop i xs of
                  [] -> error "ListSeq.lookup: bad subscript"
                  (x:_) -> x

lookupM xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
                  [] -> Nothing
                  (x:_) -> Just x

lookupWithDefault d xs i
  | i < 0 = d
  | otherwise = case drop i xs of
                  [] -> d
                  (x:_) -> x

update i y xs 
    | i < 0     = xs
    | otherwise = upd i xs
  where upd _ [] = []
        upd i (x:xs)
          | i > 0     = x : upd (i - 1) xs
          | otherwise = y : xs

adjust f i xs 
    | i < 0     = xs
    | otherwise = adj i xs
  where adj _ [] = []
        adj i (x:xs)
          | i > 0     = x : adj (i - 1) xs
          | otherwise = f x : xs

mapWithIndex f = mapi 0
  where mapi i [] = []
        mapi i (x:xs) = f i x : mapi (i + 1) xs

foldrWithIndex f e = foldi 0
  where foldi i [] = e
        foldi i (x:xs) = f i x (foldi (i + 1) xs)

foldlWithIndex f = foldi 0
  where foldi i e [] = e
        foldi i e (x:xs) = foldi (i + 1) (f e i x) xs

take i xs | i <= 0 = []
          | otherwise = Prelude.take i xs

drop i xs | i <= 0 = xs
          | otherwise = Prelude.drop i xs

splitAt i xs | i <= 0 = ([], xs)
             | otherwise = Prelude.splitAt i xs

subseq i len xs = take len (drop i xs)
        
filter = Prelude.filter
partition = List.partition
takeWhile = Prelude.takeWhile
dropWhile = Prelude.dropWhile
splitWhile = Prelude.span

zip = Prelude.zip
zip3 = Prelude.zip3
zipWith = Prelude.zipWith
zipWith3 = Prelude.zipWith3
unzip = Prelude.unzip
unzip3 = Prelude.unzip3

unzipWith f g = foldr consfg ([], [])
  where consfg a (bs, cs) = (f a : bs, g a : cs)
  -- could put ~ on tuple

unzipWith3 f g h = foldr consfgh ([], [], [])
  where consfgh a (bs, cs, ds) = (f a : bs, g a : cs, h a : ds)
  -- could put ~ on tuple

-- declare the instance

instance S.Sequence [] where
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
