-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module SequenceDefaults
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import EdisonPrelude(Maybe2(..))
import Sequence
import qualified ListSeq as L

snocUsingAppend :: Sequence s => s a -> a -> s a
snocUsingAppend s x = append s (single x)

snocUsingFoldr :: Sequence s => s a -> a -> s a
snocUsingFoldr s x = foldr cons (single x) s

appendUsingFoldr :: Sequence s => s a -> s a -> s a
appendUsingFoldr s t | null t = s
                            | otherwise = foldr cons t s

rviewDefault :: Sequence s => s a -> Maybe2 (s a) a
rviewDefault xs = if null xs then Nothing2 else Just2 (rtail xs) (rhead xs)

rtailUsingLview :: Sequence s => s a -> s a
rtailUsingLview xs = 
    case lview xs of
      Nothing2 -> empty
      Just2 x xs -> rt x xs
  where rt x xs =
          case lview xs of
            Nothing2 -> empty
            Just2 y ys -> cons x (rt y ys)

concatUsingFoldr :: Sequence s => s (s a) -> s a
concatUsingFoldr = foldr append empty

reverseUsingReverseOnto :: Sequence s => s a -> s a
reverseUsingReverseOnto s = reverseOnto s empty

reverseUsingLists :: Sequence s => s a -> s a
reverseUsingLists = fromList . L.reverse . toList

reverseOntoUsingFoldl :: Sequence s => s a -> s a -> s a
reverseOntoUsingFoldl xs ys = foldl (flip cons) ys xs

reverseOntoUsingReverse :: Sequence s => s a -> s a -> s a
reverseOntoUsingReverse = append . reverse

fromListUsingCons :: Sequence s => [a] -> s a
fromListUsingCons = L.foldr cons empty

toListUsingFoldr :: Sequence s => s a -> [a]
toListUsingFoldr = foldr (:) []

mapUsingFoldr :: Sequence s => (a -> b) -> s a -> s b
mapUsingFoldr f = foldr (cons . f) empty

concatMapUsingFoldr :: Sequence s => (a -> s b) -> s a -> s b
concatMapUsingFoldr f = foldr (append . f) empty

foldrUsingLists :: Sequence s => (a -> b -> b) -> b -> s a -> b
foldrUsingLists f e xs = L.foldr f e (toList xs)

foldlUsingLists :: Sequence s => (b -> a -> b) -> b -> s a -> b
foldlUsingLists f e xs = L.foldl f e (toList xs)

foldr1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldr1UsingLists f xs = L.foldr1 f (toList xs)

foldl1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldl1UsingLists f xs = L.foldl1 f (toList xs)

foldr1UsingLview :: Sequence s => (a -> a -> a) -> s a -> a
foldr1UsingLview f xs = 
    case lview xs of
      Nothing2 -> error (instanceName xs ++ ".foldr1: empty sequence")
      Just2 x xs -> fr1 x xs
  where fr1 x xs =
          case lview xs of
            Nothing2 -> x
            Just2 y ys -> f x (fr1 y ys)

foldl1UsingFoldl :: Sequence s => (a -> a -> a) -> s a -> a
foldl1UsingFoldl f xs = 
    case lview xs of
      Nothing2 -> error (instanceName xs ++ ".foldl1: empty sequence")
      Just2 x xs -> foldl f x xs

reducerUsingReduce1 :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducerUsingReduce1 f e s
  | null s = e
  | otherwise = f (reduce1 f s) e

reducelUsingReduce1 :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducelUsingReduce1 f e s
  | null s = e
  | otherwise = f e (reduce1 f s)

reduce1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
reduce1UsingLists f s = L.reduce1 f (toList s)

copyUsingLists :: Sequence s => Int -> a -> s a
copyUsingLists n x = fromList (L.copy n x)

tabulateUsingLists :: Sequence s => Int -> (Int -> a) -> s a
tabulateUsingLists n f = fromList (L.tabulate n f)

tabulateUsingCons :: Sequence s => Int -> (Int -> a) -> s a
tabulateUsingCons n f
    | n <= 0 = empty
    | otherwise = tab 0
  where tab i = if i == n then empty else cons (f i) (tab (i+1))


inBoundsUsingDrop :: Sequence s => s a -> Int -> Bool
inBoundsUsingDrop s i = 
  i >= 0 && not (null (drop i s))

inBoundsUsingLookupM :: Sequence s => s a -> Int -> Bool
inBoundsUsingLookupM s i =
  case lookupM s i of
    Just x -> True
    Nothing -> False

inBoundsUsingSize :: Sequence s => s a -> Int -> Bool
inBoundsUsingSize s i = i >= 0 && i < size s

lookupUsingLookupM :: Sequence s => s a -> Int -> a
lookupUsingLookupM s i =
  case lookupM s i of
    Nothing -> error (instanceName s ++ ".lookup: bad subscript")
    Just x -> x

lookupUsingDrop :: Sequence s => s a -> Int -> a
lookupUsingDrop s i
  | i < 0 || null s' = error (instanceName s ++ ".lookup: bad subscript")
  | otherwise = lhead s'
  where s' = drop i s

lookupWithDefaultUsingLookupM :: Sequence s => a -> s a -> Int -> a
lookupWithDefaultUsingLookupM d s i =
  case lookupM s i of
    Nothing -> d
    Just x -> x

lookupWithDefaultUsingDrop :: Sequence s => a -> s a -> Int -> a
lookupWithDefaultUsingDrop d s i
  | i < 0 || null s' = d
  | otherwise = lhead s'
  where s' = drop i s

lookupMUsingDrop :: Sequence s => s a -> Int -> Maybe a
lookupMUsingDrop s i
  | i < 0 || null s' = Nothing
  | otherwise = Just (lhead s')
  where s' = drop i s

filterUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingLview p xs =
  case lview xs of
    Nothing2 -> empty
    Just2 x xs -> if p x then cons x (filter p xs) else filter p xs

filterUsingLists :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingLists p xs =
  fromList (L.filter p (toList xs))

filterUsingFoldr :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingFoldr p = foldr pcons empty
  where pcons x xs = if p x then cons x xs else xs

partitionUsingLists :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
partitionUsingLists p xs =
  let (ys,zs) = L.partition p (toList xs)
  in (fromList ys, fromList zs)

partitionUsingFoldr :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
partitionUsingFoldr p = foldr pcons (empty, empty)
  where pcons x (xs, xs') = if p x then (cons x xs, xs') else (xs, cons x xs')

updateUsingAdjust :: Sequence s => Int -> a -> s a -> s a
updateUsingAdjust i y = adjust (const y) i

updateUsingSplitAt :: Sequence s => Int -> a -> s a -> s a
updateUsingSplitAt i x xs
  | i < 0 = xs
  | otherwise = let (ys,zs) = splitAt i xs
                in if null zs then xs else append ys (cons x (ltail zs))

adjustUsingLists :: Sequence s => (a -> a) -> Int -> s a -> s a
adjustUsingLists f i xs = fromList (L.adjust f i (toList xs))

adjustUsingSplitAt :: Sequence s => (a -> a) -> Int -> s a -> s a
adjustUsingSplitAt f i xs
  | i < 0 = xs
  | otherwise = let (ys,zs) = splitAt i xs
                in case lview zs of
                     Nothing2 -> xs
                     Just2 z zs' -> append ys (cons (f z) zs')

{-
insertAtUsingLists :: Sequence s => Int -> a -> s a -> s a
insertAtUsingLists i x xs = 
  fromList (L.insertAt i x (toList xs))

insertAtUsingSplitAt :: Sequence s => Int -> a -> s a -> s a
insertAtUsingSplitAt i x xs
  | (xs_before, xs_after) <- splitAt i xs =
      append xs_before (cons x xs_after)

deleteAtUsingLists :: Sequence s => Int -> s a -> s a
deleteAtUsingLists i xs = fromList (L.deleteAt i (toList xs))

deleteAtUsingSplitAt :: Sequence s => Int -> s a -> s a
deleteAtUsingSplitAt i xs
  | (xs_before, xs_after) <- splitAt i xs =
      append xs_before (ltail xs_after)
-}

mapWithIndexUsingLists :: Sequence s => (Int -> a -> b) -> s a -> s b
mapWithIndexUsingLists f xs = fromList (L.mapWithIndex f (toList xs))

foldrWithIndexUsingLists :: 
  Sequence s => (Int -> a -> b -> b) -> b -> s a -> b
foldrWithIndexUsingLists f e xs = L.foldrWithIndex f e (toList xs)

foldlWithIndexUsingLists :: 
  Sequence s => (b -> Int -> a -> b) -> b -> s a -> b
foldlWithIndexUsingLists f e xs = L.foldlWithIndex f e (toList xs)

takeUsingLists :: Sequence s => Int -> s a -> s a
takeUsingLists i s = fromList (L.take i (toList s))

takeUsingLview :: Sequence s => Int -> s a -> s a
takeUsingLview i xs
  | i <= 0 = empty
  | otherwise = case lview xs of
                  Nothing2 -> empty
                  Just2 x xs' -> cons x (take (i-1) xs')

dropUsingLists :: Sequence s => Int -> s a -> s a
dropUsingLists i s = fromList (L.drop i (toList s))

dropUsingLtail :: Sequence s => Int -> s a -> s a
dropUsingLtail i xs
  | i <= 0 || null xs = xs
  | otherwise = dropUsingLtail (i-1) (ltail xs)

splitAtDefault :: Sequence s => Int -> s a -> (s a, s a)
splitAtDefault i s = (take i s, drop i s)

splitAtUsingLview :: Sequence s => Int -> s a -> (s a, s a)
splitAtUsingLview i xs
  | i <= 0 = (empty,xs)
  | otherwise = case lview xs of
                  Nothing2 -> (empty,empty)
                  Just2 x xs' -> (cons x ys,zs)
                    where (ys,zs) = splitAtUsingLview (i-1) xs'

subseqDefault :: Sequence s => Int -> Int -> s a -> s a
subseqDefault i len xs = take len (drop i xs)

takeWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
takeWhileUsingLview p xs =
  case lview xs of
    Just2 x xs' | p x -> cons x (takeWhileUsingLview p xs')
    _ -> empty

dropWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
dropWhileUsingLview p xs =
  case lview xs of
    Just2 x xs' | p x -> dropWhileUsingLview p xs'
    _ -> xs

splitWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
splitWhileUsingLview p xs =
  case lview xs of
    Just2 x xs' | p x -> let (front, back) = splitWhileUsingLview p xs'
                         in (cons x front, back)
    _ -> (empty, xs)

zipUsingLview :: Sequence s => s a -> s b -> s (a,b)
zipUsingLview xs ys =
  case lview xs of
    Nothing2 -> empty
    Just2 x xs' ->
      case lview ys of
        Nothing2 -> empty
        Just2 y ys' -> cons (x,y) (zipUsingLview xs' ys')

zip3UsingLview :: Sequence s => s a -> s b -> s c -> s (a,b,c)
zip3UsingLview xs ys zs =
  case lview xs of
    Nothing2 -> empty
    Just2 x xs' ->
      case lview ys of
        Nothing2 -> empty
        Just2 y ys' ->
          case lview zs of
            Nothing2 -> empty
            Just2 z zs' -> cons (x,y,z) (zip3UsingLview xs' ys' zs')

zipWithUsingLview :: Sequence s => (a -> b -> c) -> s a -> s b -> s c
zipWithUsingLview f xs ys =
  case lview xs of
    Nothing2 -> empty
    Just2 x xs' ->
      case lview ys of
        Nothing2 -> empty
        Just2 y ys' -> cons (f x y) (zipWithUsingLview f xs' ys')

zipWith3UsingLview :: 
  Sequence s => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3UsingLview f xs ys zs =
  case lview xs of
    Nothing2 -> empty
    Just2 x xs' ->
      case lview ys of
        Nothing2 -> empty
        Just2 y ys' ->
          case lview zs of
            Nothing2 -> empty
            Just2 z zs' -> cons (f x y z) (zipWith3UsingLview f xs' ys' zs')

zipUsingLists :: Sequence s => s a -> s b -> s (a,b)
zipUsingLists xs ys = fromList (L.zip (toList xs) (toList ys))

zip3UsingLists :: Sequence s => s a -> s b -> s c -> s (a,b,c)
zip3UsingLists xs ys zs = 
  fromList (L.zip3 (toList xs) (toList ys) (toList zs))

zipWithUsingLists :: Sequence s => (a -> b -> c) -> s a -> s b -> s c
zipWithUsingLists f xs ys =
  fromList (L.zipWith f (toList xs) (toList ys))

zipWith3UsingLists :: 
  Sequence s => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3UsingLists f xs ys zs =
  fromList (L.zipWith3 f (toList xs) (toList ys) (toList zs))

unzipUsingLists :: Sequence s => s (a,b) -> (s a, s b)
unzipUsingLists xys =
  case L.unzip (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

unzipUsingFoldr :: Sequence s => s (a,b) -> (s a, s b)
unzipUsingFoldr = foldr pcons (empty,empty) 
  where pcons (x,y) (xs,ys) = (cons x xs,cons y ys)

unzip3UsingLists :: Sequence s => s (a,b,c) -> (s a, s b, s c)
unzip3UsingLists xyzs =
  case L.unzip3 (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

unzip3UsingFoldr :: Sequence s => s (a,b,c) -> (s a, s b, s c)
unzip3UsingFoldr = foldr tcons (empty,empty,empty)
  where tcons (x,y,z) (xs,ys,zs) = (cons x xs,cons y ys,cons z zs)

unzipWithUsingLists :: 
  Sequence s => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWithUsingLists f g xys =
  case L.unzipWith f g (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

unzipWithUsingFoldr :: 
  Sequence s => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWithUsingFoldr f g = foldr pcons (empty,empty) 
  where pcons e (xs,ys) = (cons (f e) xs,cons (g e) ys)

unzipWith3UsingLists :: 
  Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3UsingLists f g h xyzs =
  case L.unzipWith3 f g h (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

unzipWith3UsingFoldr :: 
  Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3UsingFoldr f g h = foldr tcons (empty,empty,empty) 
  where tcons e (xs,ys,zs) = (cons (f e) xs,cons (g e) ys,cons (h e) zs)

