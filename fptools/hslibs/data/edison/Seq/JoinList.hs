-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module JoinList
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of join lists
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

moduleName = "JoinList"

data Seq a = E | L a | A (Seq a) (Seq a)
  -- invariant: E never a child of A

half :: Int -> Int
half n = n `div` 2

empty = E
single = L

cons x E = L x
cons x xs = A (L x) xs

snoc E x = L x
snoc xs x = A xs (L x)

append E ys = ys
append xs E = xs
append xs ys = A xs ys


-- path reversal on lview/ltail

lview E = Nothing2
lview (L x) = Just2 x E
lview (A xs ys) = lvw xs ys
  where lvw E zs = error "JoinList.lvw: bug"
        lvw (L x) zs = Just2 x zs
        lvw (A xs ys) zs = lvw xs (A ys zs)

lhead E = error "JoinList.lhead: empty sequence"
lhead (L x) = x
lhead (A xs ys) = lhead xs

ltail E = E
ltail (L x) = E
ltail (A xs ys) = ltl xs ys
  where ltl E zs = error "JoinList.ltl: bug"
        ltl (L x) zs = zs
        ltl (A xs ys) zs = ltl xs (A ys zs)

-- Don't want to do plain path reversal on rview/rtail because of expectation
-- that left accesses are more common, so we would prefer to keep the left
-- spine short.

rview E = Nothing2
rview (L x) = Just2 E x
rview (A xs ys) = rvw xs ys
  where rvw xs (A ys (A zs s)) = rvw (A xs (A ys zs)) s
        rvw xs (A ys (L x)) = Just2 (A xs ys) x
        rvw xs (L x) = Just2 xs x
        rvw xs _ = error "JoinList.rvw: bug"
 
rhead E = error "JoinList.rhead: empty sequence"
rhead (L x) = x
rhead (A xs ys) = rhead ys

rtail E = E
rtail (L x) = E
rtail (A xs ys) = rtl xs ys
  where rtl xs (A ys (A zs s)) = A (A xs ys) (rtl zs s)
        rtl xs (A ys (L _)) = A xs ys
        rtl xs (L x) = xs
        rtl xs _ = error "JoinList.rtl: bug"

null E = True
null _ = False

size xs = sz xs (0::Int)
  where sz E n = n
        sz (L x) n = n + (1::Int)
        sz (A xs ys) n = sz xs (sz ys n)

reverse (A xs ys) = A (reverse ys) (reverse xs)
reverse xs = xs -- L x or E

toList xs = tol xs []
  where tol E rest = rest
        tol (L x) rest = x:rest
        tol (A xs ys) rest = tol xs (tol ys rest)

map f E = E
map f (L x) = L (f x)
map f (A xs ys) = A (map f xs) (map f ys)

foldr f e E = e
foldr f e (L x) = f x e
foldr f e (A xs ys) = foldr f (foldr f e ys) xs

foldl f e E = e
foldl f e (L x) = f e x
foldl f e (A xs ys) = foldl f (foldl f e xs) ys

foldr1 f E = error "JoinList.foldr1: empty sequence"
foldr1 f (L x) = x
foldr1 f (A xs ys) = foldr f (foldr1 f ys) xs

foldl1 f E = error "JoinList.foldl1: empty sequence"
foldl1 f (L x) = x
foldl1 f (A xs ys) = foldl f (foldl1 f xs) ys

copy n x 
    | n <= 0 = E
    | otherwise = cpy n x
  where cpy n x  -- n > 0
          | even n = let xs = cpy (half n) x
                     in A xs xs
          | n == 1 = L x
          | otherwise = let xs = cpy (half n) x
                        in A (L x) (A xs xs)

tabulate n f
    | n <= 0 = E
    | otherwise = tab 0
  where m = n-1
        tab i = if i == m then L (f i) else A (L (f i)) (tab (i+1))
        
-- the remaining functions all use defaults

concat = concatUsingFoldr
reverseOnto = reverseOntoUsingReverse
fromList = fromListUsingCons
concatMap = concatMapUsingFoldr

reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists

inBounds = inBoundsUsingDrop
lookup = lookupUsingDrop
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingDrop

update = updateUsingSplitAt
adjust = adjustUsingSplitAt

mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists

take = takeUsingLview
drop = dropUsingLtail
splitAt = splitAtUsingLview
subseq = subseqDefault
        
filter = filterUsingLview
partition = partitionUsingFoldr
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview

zip = zipUsingLview
zip3 = zip3UsingLview
zipWith = zipWithUsingLview
zipWith3 = zipWith3UsingLview

unzip = unzipUsingFoldr
unzip3 = unzip3UsingFoldr
unzipWith = unzipWithUsingFoldr
unzipWith3 = unzipWith3UsingFoldr

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
  xs == ys = toList xs == toList ys

instance Show a => Show (Seq a) where
  show xs = show (toList xs)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = sized arbTree
    where arbTree 0 = return E
          arbTree 1 = liftM L arbitrary
          arbTree n =
            frequency [(1, liftM L arbitrary),
                       (4, liftM2 A (arbTree (n `div` 2))
                                    (arbTree (n `div` 2)))]

  coarbitrary E = variant 0
  coarbitrary (L x) = variant 1 . coarbitrary x
  coarbitrary (A xs ys) = variant 2 . coarbitrary xs . coarbitrary ys
