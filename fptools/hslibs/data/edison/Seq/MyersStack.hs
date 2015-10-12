-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module MyersStack
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

moduleName = "MyersStack"

-- Adapted from
--   Eugene Myers. "An applicative random-access stack". Information
--   Processing Letters, 17(5):241-248, December 1983.

data Seq a = E | C !Int a (Seq a) (Seq a)
  -- what about strictness flags on tail and jump-tail?

-- auxiliary function
jump (C _ _ _ (C _ _ _ xs')) = xs'

empty = E
single x = C 1 x E E

cons x xs@(C i _  _  (C j _ _ xs'))
    | i == j = C (1 + i + j) x xs xs'
cons x xs = C 1 x xs xs

lview E = Nothing2
lview (C _ x xs _) = Just2 x xs

lhead E = error "MyersStack.lhead: empty sequence"
lhead (C _ x xs _) = x

ltail E = E
ltail (C _ x xs _) = xs

rview E = Nothing2
rview xs = Just2 (rtail xs) (rhead xs)

rhead E = error "MyersStack.rhead: empty sequence"
rhead (C _ x xs xs') = rh x xs xs'
  where rh x xs (C _ y ys ys') = rh y ys ys'
        rh x (C _ y ys ys') E = rh y ys ys'
        rh x E E = x

rtail E = E
rtail (C _ x xs _) = rt x xs
  where rt y E = E
        rt y (C _ x xs _) = cons y (rt x xs)

null E = True
null _ = False

size xs = go xs
  where go E = (0::Int)
        go (C j x xs xs') = j + size xs'

reverseOnto E ys = ys
reverseOnto (C _ x xs _) ys = reverseOnto xs (cons x ys)

map f E = E
map f (C j x xs xs')
    | j == 1    = C j (f x) ys ys
    | otherwise = C j (f x) ys (jump ys)
  where ys = map f xs

foldr f e E = e
foldr f e (C _ x xs _) = f x (foldr f e xs)

foldl f e E = e
foldl f e (C _ x xs _) = foldl f (f e x) xs

foldr1 f E = error "MyersStack.foldr1: empty sequence"
foldr1 f (C _ x xs _) = fr x xs
  where fr y E = y
        fr y (C _ x xs _) = f y (fr x xs)

foldl1 f E = error "MyersStack.foldl1: empty sequence"
foldl1 f (C _ x xs _) = foldl f x xs

inBounds xs i = inb xs i
  where inb E i = False
        inb (C j x xs xs') i
          | i < j     = (i >= 0)
          | otherwise = inb xs' (i - j)

lookup xs i = look xs i
  where look E i = error "MyersStack.lookup: bad subscript"
        look (C j x xs xs') i
          | i >= j   = look xs' (i - j)
          | i > 0    = look xs  (i - 1)
          | i == 0   = x
          | otherwise = error "MyersStack.lookup: bad subscript"

lookupM xs i = look xs i
  where look E i = Nothing
        look (C j x xs xs') i
          | i >= j   = look xs' (i - j)
          | i > 0    = look xs  (i - 1)
          | i == 0   = Just x
          | otherwise = Nothing

lookupWithDefault d xs i = look xs i
  where look E i = d
        look (C j x xs xs') i
          | i >= j   = look xs' (i - j)
          | i > 0    = look xs  (i - 1)
          | i == 0   = x
          | otherwise = d

update i y xs = upd i xs
  where upd i E = E
        upd 0 (C j x xs xs') = C j y xs xs'
        upd i (C j x xs _)
            | j == 1    = C j x ys ys
            | otherwise = C j x ys (jump ys)
          where ys = upd (i - 1) xs

adjust f i xs = adj i xs
  where adj i E = E
        adj 0 (C j x xs xs') = C j (f x) xs xs'
        adj i (C j x xs _)
            | j == 1    = C j x ys ys
            | otherwise = C j x ys (jump ys)
          where ys = adj (i - (1::Int)) xs

drop n xs = drp n xs
  where drp n xs | n <= 0 = xs
        drp n E = E
        drp n (C j x xs xs')
          | n < j     = drp (n - 1) xs
          | otherwise = drp (n - j) xs'

unzip E = (E, E)
unzip (C j (x,y) ps ps')
    | j == 1    = (C j x xs xs, C j y ys ys)
    | otherwise = (C j x xs (jump xs), C j y ys (jump ys))
  where (xs,ys) = unzip ps

unzip3 E = (E, E, E)
unzip3 (C j (x,y,z) ts ts')
    | j == 1    = (C j x xs xs, C j y ys ys, C j z zs zs)
    | otherwise = (C j x xs (jump xs), C j y ys (jump ys), C j z zs (jump zs))
  where (xs,ys,zs) = unzip3 ts

unzipWith f g E = (E, E)
unzipWith f g (C j x xs _)
    | j == 1    = (C j (f x) as as, C j (g x) bs bs)
    | otherwise = (C j (f x) as (jump as), C j (g x) bs (jump bs))
  where (as,bs) = unzipWith f g xs

unzipWith3 f g h E = (E, E, E)
unzipWith3 f g h (C j x xs _)
    | j == 1    = (C j (f x) as as, C j (g x) bs bs, C j (h x) cs cs)
    | otherwise = (C j (f x) as (jump as), C j (g x) bs (jump bs),
                   C j (h x) cs (jump cs))
  where (as,bs,cs) = unzipWith3 f g h xs

-- the remaining functions all use defaults

snoc = snocUsingFoldr
append = appendUsingFoldr
concat = concatUsingFoldr
reverse = reverseUsingReverseOnto
fromList = fromListUsingCons
toList = toListUsingFoldr
concatMap = concatMapUsingFoldr
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
copy = copyUsingLists
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
  xs == ys =
    (size xs == size ys) && (toList xs == toList ys)

instance Show a => Show (Seq a) where
  show xs = show (toList xs)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 return (fromList xs)

  coarbitrary xs = coarbitrary (toList xs)

-------------

{-
questions:
  - any benefit to
      E | C1 x xs | CJ Int# x xs xs'

  - any benefit to length instead of delta?

  - any benefit to delta not counting x (i.e., base 0 instead of base 1)?

I don't believe any will do any better, except possibly the first
-}
