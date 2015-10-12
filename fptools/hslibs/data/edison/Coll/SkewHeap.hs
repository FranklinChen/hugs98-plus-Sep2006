-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module SkewHeap
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of skew heaps
    Heap, -- instance of Coll/CollX, OrdColl/OrdCollX

    -- CollX operations
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,

    -- Coll operations
    toSeq, lookup, lookupM, lookupAll, lookupWithDefault, fold, fold1,
    filter, partition,

    -- OrdCollX operations
    deleteMin,deleteMax,unsafeInsertMin,unsafeInsertMax,unsafeFromOrdSeq,
    unsafeAppend,filterLT,filterLE,filterGT,filterGE,partitionLT_GE,
    partitionLE_GT,partitionLT_GT,

    -- OrdColl operations
    minView,minElem,maxView,maxElem,foldr,foldl,foldr1,foldl1,toOrdSeq,

    -- other supported operations
    unsafeMapMonotonic,

    -- documentation
    moduleName,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(..)
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import EdisonPrelude(Maybe2(..))
import qualified Collection as C
import qualified Sequence as S
import CollectionDefaults
import Monad
import QuickCheck

moduleName = "SkewHeap"

-- Adapted from
--   Daniel Sleator and Robert Tarjan. "Self-Adjusting Heaps".
--   SIAM Journal on Computing, 15(1):52-69, February 1986.

data Heap a = E | T a (Heap a) (Heap a)

{-
For delete,deleteAll,filter,partition: could compute fringe and reduce
rather that rebuilding with union at every deleted node
-}

empty :: Ord a => Heap a
empty = E

single :: Ord a => a -> Heap a
single x = T x E E

insert :: Ord a => a -> Heap a -> Heap a
insert x E = T x E E
insert x h@(T y a b)
  | x <= y    = T x h E
  | otherwise = T y (insert x b) a

union :: Ord a => Heap a -> Heap a -> Heap a
union E h = h
union h@(T x a b) h' = union' h x a b h'
  where union' h x a b E = h
        union' hx x a b hy@(T y c d)
          | x <= y    = T x (union' hy y c d b) a
          | otherwise = T y (union' hx x a b d) c

delete :: Ord a => a -> Heap a -> Heap a
delete x h = case del h of
               Just h' -> h'
               Nothing -> h
  where del (T y a b) =
          case compare x y of
            LT -> Nothing
            EQ -> Just (union a b)
            GT -> case del b of
                    Just b' -> Just (T y a b')
                    Nothing -> case del a  of
                                 Just a' -> Just (T y a' b)
                                 Nothing -> Nothing
        del E = Nothing

deleteAll :: Ord a => a -> Heap a -> Heap a
deleteAll x h@(T y a b) =
  case compare x y of
    LT -> h
    EQ -> union (deleteAll x a) (deleteAll x b)
    GT -> T y (deleteAll x a) (deleteAll x b)
deleteAll x E = E

null :: Ord a => Heap a -> Bool
null E = True
null _ = False

size :: Ord a => Heap a -> Int
size h = sz h 0
  where sz E i = i
        sz (T _ a b) i = sz a (sz b (i + 1))

member :: Ord a => Heap a -> a -> Bool
member E x = False
member (T y a b) x =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member b x || member a x

count :: Ord a => Heap a -> a -> Int
count E x = 0
count (T y a b) x =
  case compare x y of
    LT -> 0
    EQ -> 1 + count b x + count a x
    GT -> count b x + count a x

toSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toSeq h = tol h S.empty
  where tol E rest = rest
        tol (T x a b) rest = S.cons x (tol b (tol a rest))

lookupM :: Ord a => Heap a -> a -> Maybe a
lookupM E x = Nothing
lookupM (T y a b) x =
  case compare x y of
    LT -> Nothing
    EQ -> Just y
    GT -> lookupM b x `mplus` lookupM a x

lookupAll :: (Ord a,S.Sequence seq) => Heap a -> a -> seq a
lookupAll h x = look h S.empty
  where look E ys = ys
        look (T y a b) ys =
          case compare x y of
            LT -> ys
            EQ -> S.cons y (look b (look a ys))
            GT -> look b (look a ys)

fold :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold f e E = e
fold f e (T x a b) = f x (fold f (fold f e a) b)

fold1 :: Ord a => (a -> a -> a) -> Heap a -> a
fold1 f E = error "SkewHeap.fold1: empty collection"
fold1 f (T x a b) = fold f (fold f x a) b

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter p E = E
filter p (T x a b)
    | p x = T x (filter p a) (filter p b)
    | otherwise = union (filter p a) (filter p b)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition p E = (E, E)
partition p (T x a b)
    | p x = (T x a' b', union a'' b'')
    | otherwise = (union a' b', T x a'' b'')
  where (a', a'') = partition p a
        (b', b'') = partition p b


deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T x a b) = union a b

deleteMax :: Ord a => Heap a -> Heap a
deleteMax h = case maxView h of
                Nothing2 -> E
                Just2 h' x -> h'

unsafeInsertMin :: Ord a => a -> Heap a -> Heap a
unsafeInsertMin x h = T x h E

unsafeAppend :: Ord a => Heap a -> Heap a -> Heap a
unsafeAppend E h = h
unsafeAppend (T x a b) h = T x (unsafeAppend b h) a

filterLT :: Ord a => a -> Heap a -> Heap a
filterLT y (T x a b) | x < y = T x (filterLT y a) (filterLT y b)
filterLT y _ = E

filterLE :: Ord a => a -> Heap a -> Heap a
filterLE y (T x a b) | x <= y = T x (filterLE y a) (filterLE y b)
filterLE y _ = E

filterGT :: Ord a => a -> Heap a -> Heap a
filterGT y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(T x a b) hs
          | x > y = h : hs
          | otherwise = collect a (collect b hs)

filterGE :: Ord a => a -> Heap a -> Heap a
filterGE y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(T x a b) hs
          | x >= y = h : hs
          | otherwise = collect b (collect a hs)

partitionLT_GE :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GE y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs
          | x >= y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (T x a' b', hs'')

partitionLE_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLE_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs
          | x > y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (T x a' b', hs'')

partitionLT_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs = 
          case compare x y of
            GT -> (E, h:hs)
            EQ -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (union a' b', hs'')
            LT -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (T x a' b', hs'')

minView :: Ord a => Heap a -> Maybe2 a (Heap a)
minView E = Nothing2
minView (T x a b) = Just2 x (union a b)

minElem :: Ord a => Heap a -> a
minElem E = error "SkewHeap.minElem: empty collection"
minElem (T x a b) = x

maxView :: Ord a => Heap a -> Maybe2 (Heap a) a
maxView E = Nothing2
maxView (T x E E) = Just2 E x
maxView (T x a E) = Just2 (T x a' E) y
  where Just2 a' y = maxView a
maxView (T x E a) = Just2 (T x a' E) y
  where Just2 a' y = maxView a
maxView (T x a b)
    | y >= z    = Just2 (T x a' b) y
    | otherwise = Just2 (T x a b') z
  where Just2 a' y = maxView a
        Just2 b' z = maxView b

-- warning: maxView and maxElem may disagree if root is equal to max!

maxElem :: Ord a => Heap a -> a
maxElem E = error "SkewHeap.maxElem: empty collection"
maxElem (T x E E) = x
maxElem (T x a E) = maxElem a
maxElem (T x E a) = maxElem a
maxElem (T x a b) = findMax b (findLeaf a)
  where findMax E m = m
        findMax (T x E E) m
          | m >= x = m
          | otherwise = x
        findMax (T x a E) m = findMax a m
        findMax (T x E a) m = findMax a m
        findMax (T x a b) m = findMax a (findMax b m)

        findLeaf E = error "SkewHeap.maxElem: bug"
        findLeaf (T x E E) = x
        findLeaf (T x a E) = findLeaf a
        findLeaf (T x E a) = findLeaf a
        findLeaf (T x a b) = findMax b (findLeaf a)

foldr :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr f e E = e
foldr f e (T x a b) = f x (foldr f e (union a b))

foldl :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl f e E = e
foldl f e (T x a b) = foldl f (f e x) (union a b)

foldr1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1 f E = error "SkewHeap.foldr1: empty collection"
foldr1 f (T x E E) = x
foldr1 f (T x a b) = f x (foldr1 f (union a b))

foldl1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1 f E = error "SkewHeap.foldl1: empty collection"
foldl1 f (T x a b) = foldl f x (union a b)

{- ???? -}
unsafeMapMonotonic :: Ord a => (a -> a) -> Heap a -> Heap a
unsafeMapMonotonic f E = E
unsafeMapMonotonic f (T x a b) =
  T (f x) (unsafeMapMonotonic f a) (unsafeMapMonotonic f b)

-- the remaining functions all use default definitions

fromSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
fromSeq = fromSeqUsingUnionSeq

insertSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
insertSeq = insertSeqUsingUnion

unionSeq :: (Ord a,S.Sequence seq) => seq (Heap a) -> Heap a
unionSeq = unionSeqUsingReduce

deleteSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
deleteSeq = deleteSeqUsingDelete

lookup :: Ord a => Heap a -> a -> a
lookup = lookupUsingLookupM

lookupWithDefault :: Ord a => a -> Heap a -> a -> a
lookupWithDefault = lookupWithDefaultUsingLookupM

unsafeInsertMax :: Ord a => Heap a -> a -> Heap a
unsafeInsertMax = unsafeInsertMaxUsingUnsafeAppend

unsafeFromOrdSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
unsafeFromOrdSeq = unsafeFromOrdSeqUsingUnsafeInsertMin

toOrdSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toOrdSeq = toOrdSeqUsingFoldr

-- instance declarations

instance Ord a => C.CollX Heap a where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   instanceName c = moduleName}

instance Ord a => C.OrdCollX Heap a where
  {deleteMin = deleteMin; deleteMax = deleteMax; 
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax; 
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend; 
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT; 
   filterGE = filterGE; partitionLT_GE = partitionLT_GE; 
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Ord a => C.Coll Heap a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM; 
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault; 
   fold = fold; fold1 = fold1; filter = filter; partition = partition}

instance Ord a => C.OrdColl Heap a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldl = foldl; foldr1 = foldr1; 
   foldl1  = foldl1; toOrdSeq = toOrdSeq}

instance Ord a => Eq (Heap a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Heap a) where
  show xs = show (C.toOrdList xs)

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized (\n -> arbTree n)
    where arbTree 0 = return E
          arbTree n =
            frequency [(1, return E),
                       (4, liftM3 sift arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 4)))]

          sift x s@(T y a b) E
            | y < x = T y (sift x a b) E
          sift x E s@(T y a b)
            | y < x = T y E (sift x a b)
          sift x s@(T y a b) t@(T z c d)
            | y < x && y <= z = T y (sift x a b) t
            | z < x           = T z s (sift x c d)
          sift x a b = T x a b

  coarbitrary E = variant 0
  coarbitrary (T x a b) = 
      variant 1 . coarbitrary x . coarbitrary a . coarbitrary b
