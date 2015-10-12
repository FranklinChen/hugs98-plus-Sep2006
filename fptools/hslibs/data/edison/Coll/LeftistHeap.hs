-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module LeftistHeap
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of leftist heaps
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
import qualified Collection as C ( CollX(..), OrdCollX(..), Coll(..), OrdColl(..), 
                                   unionList, toOrdList )
import qualified Sequence as S
import CollectionDefaults
import Monad
import QuickCheck

moduleName = "LeftistHeap"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 3.1.

data Heap a = E | L !Int a !(Heap a) !(Heap a)
  -- want to say !a, but would need Eval a context

node x a E = L 1 x a E
node x E b = L 1 x b E
node x a@(L m _ _ _) b@(L n _ _ _)
  | m <= n     = L (m + 1) x b a
  | otherwise  = L (n + 1) x a b

{-
Note: when we want to recurse down both sides, and we have a choice,
recursing down the smaller side first will minimize stack usage.

For delete,deleteAll,filter,partition: could compute fringe and reduce
rather that rebuilding with union at every deleted node
-}

empty :: Ord a => Heap a
empty = E

single :: Ord a => a -> Heap a
single x = L 1 x E E

insert :: Ord a => a -> Heap a -> Heap a
insert x E = L 1 x E E
insert x h@(L m y a b)
  | x <= y    = L 1 x h E
  | otherwise = node y a (insert x b)

union :: Ord a => Heap a -> Heap a -> Heap a
union E h = h
union h@(L _ x a b) h' = union' h x a b h'
  where union' h x a b E = h
        union' hx x a b hy@(L _ y c d)
          | x <= y    = node x a (union' hy y c d b)
          | otherwise = node y c (union' hx x a b d)

{-
union E h = h
union h E = h
union h1@(L _ x a b) h2@(L _ y c d)
  | x <= y    = node x a (union b h2)
  | otherwise = node y c (union h1 d)
    -- ??? optimize to catch fact that h1 or h2 is known to be L case?
-}

delete :: Ord a => a -> Heap a -> Heap a
delete x h = case del h of
               Just h' -> h'
               Nothing -> h
  where del (L _ y a b) =
          case compare x y of
            LT -> Nothing
            EQ -> Just (union a b)
            GT -> case del b of
                    Just b' -> Just (node y a b')
                    Nothing -> case del a  of
                                 Just a' -> Just (node y a' b)
                                 Nothing -> Nothing
        del E = Nothing

deleteAll :: Ord a => a -> Heap a -> Heap a
deleteAll x h@(L _ y a b) =
  case compare x y of
    LT -> h
    EQ -> union (deleteAll x a) (deleteAll x b)
    GT -> node y (deleteAll x a) (deleteAll x b)
deleteAll x E = E

null :: Ord a => Heap a -> Bool
null E = True
null _ = False

size :: Ord a => Heap a -> Int
size h = sz h 0
  where sz E i = i
        sz (L _ _ a b) i = sz a (sz b (i + 1))

member :: Ord a => Heap a -> a -> Bool
member E x = False
member (L _ y a b) x =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member b x || member a x

count :: Ord a => Heap a -> a -> Int
count E x = 0
count (L _ y a b) x =
  case compare x y of
    LT -> 0
    EQ -> 1 + count b x + count a x
    GT -> count b x + count a x

toSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toSeq h = tol h S.empty
  where tol E rest = rest
        tol (L _ x a b) rest = S.cons x (tol b (tol a rest))

lookupM :: Ord a => Heap a -> a -> Maybe a
lookupM E x = Nothing
lookupM (L _ y a b) x =
  case compare x y of
    LT -> Nothing
    EQ -> Just y
    GT -> lookupM b x `mplus` lookupM a x

lookupAll :: (Ord a,S.Sequence seq) => Heap a -> a -> seq a
lookupAll h x = look h S.empty
  where look E ys = ys
        look (L _ y a b) ys =
          case compare x y of
            LT -> ys
            EQ -> S.cons y (look b (look a ys))
            GT -> look b (look a ys)

fold :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold f e E = e
fold f e (L _ x a b) = f x (fold f (fold f e a) b)

fold1 :: Ord a => (a -> a -> a) -> Heap a -> a
fold1 f E = error "LeftistHeap.fold1: empty collection"
fold1 f (L _ x a b) = fold f (fold f x a) b

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter p E = E
filter p (L _ x a b)
    | p x = node x (filter p a) (filter p b)
    | otherwise = union (filter p a) (filter p b)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition p E = (E, E)
partition p (L _ x a b)
    | p x = (node x a' b', union a'' b'')
    | otherwise = (union a' b', node x a'' b'')
  where (a', a'') = partition p a
        (b', b'') = partition p b


deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (L _ x a b) = union a b

deleteMax :: Ord a => Heap a -> Heap a
deleteMax h = case maxView h of
                Nothing2 -> E
                Just2 h' x -> h'

unsafeInsertMin :: Ord a => a -> Heap a -> Heap a
unsafeInsertMin x h = L 1 x h E

unsafeAppend :: Ord a => Heap a -> Heap a -> Heap a
unsafeAppend E h = h
unsafeAppend (L _ y a b) h = node y a (unsafeAppend b h)

filterLT :: Ord a => a -> Heap a -> Heap a
filterLT y (L _ x a b) | x < y = node x (filterLT y a) (filterLT y b)
filterLT y _ = E

filterLE :: Ord a => a -> Heap a -> Heap a
filterLE y (L _ x a b) | x <= y = node x (filterLE y a) (filterLE y b)
filterLE y _ = E

filterGT :: Ord a => a -> Heap a -> Heap a
filterGT y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(L _ x a b) hs
          | x > y = h : hs
          | otherwise = collect a (collect b hs)

filterGE :: Ord a => a -> Heap a -> Heap a
filterGE y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(L _ x a b) hs
          | x >= y = h : hs
          | otherwise = collect b (collect a hs)

partitionLT_GE :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GE y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(L _ x a b) hs
          | x >= y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (node x a' b', hs'')

partitionLE_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLE_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(L _ x a b) hs
          | x > y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (node x a' b', hs'')

partitionLT_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(L _ x a b) hs = 
          case compare x y of
            GT -> (E, h:hs)
            EQ -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (union a' b', hs'')
            LT -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (node x a' b', hs'')

minView :: Ord a => Heap a -> Maybe2 a (Heap a)
minView E = Nothing2
minView (L _ x a b) = Just2 x (union a b)

minElem :: Ord a => Heap a -> a
minElem E = error "LeftistHeap.minElem: empty collection"
minElem (L _ x a b) = x

maxView :: Ord a => Heap a -> Maybe2 (Heap a) a
maxView E = Nothing2
maxView (L _ x E _) = Just2 E x
maxView (L _ x a E) = Just2 (L 1 x a' E) y
  where Just2 a' y = maxView a
maxView (L _ x a b)
    | y >= z    = Just2 (node x a' b) y
    | otherwise = Just2 (node x a b') z
  where Just2 a' y = maxView a
        Just2 b' z = maxView b

-- warning: maxView and maxElem may disagree if root is equal to max!

maxElem :: Ord a => Heap a -> a
maxElem E = error "LeftistHeap.maxElem: empty collection"
maxElem (L _ x E _) = x
maxElem (L _ x a b) = findMax b (findLeaf a)
  where findMax E m = m
        findMax (L _ x E _) m
          | m >= x = m
          | otherwise = x
        findMax (L _ x a b) m = findMax a (findMax b m)

        findLeaf E = error "LeftistHeap.maxElem: bug"
        findLeaf (L _ x E _) = x
        findLeaf (L _ x a b) = findMax b (findLeaf a)

foldr :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr f e E = e
foldr f e (L _ x a b) = f x (foldr f e (union a b))

foldl :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl f e E = e
foldl f e (L _ x a b) = foldl f (f e x) (union a b)

foldr1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1 f E = error "LeftistHeap.foldr1: empty collection"
foldr1 f (L _ x E _) = x
foldr1 f (L _ x a b) = f x (foldr1 f (union a b))

foldl1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1 f E = error "LeftistHeap.foldl1: empty collection"
foldl1 f (L _ x a b) = foldl f x (union a b)

{- ???? -}
unsafeMapMonotonic :: Ord a => (a -> a) -> Heap a -> Heap a
unsafeMapMonotonic f E = E
unsafeMapMonotonic f (L i x a b) =
  L i (f x) (unsafeMapMonotonic f a) (unsafeMapMonotonic f b)

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
   foldl1 = foldl1; toOrdSeq = toOrdSeq}

instance Ord a => Eq (Heap a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Heap a) where
  show xs = show (C.toOrdList xs)

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized (\n -> arbTree n)
    where arbTree 0 = return E
          arbTree n =
            frequency [(1, return E),
                       (4, liftM3 snode arbitrary (arbTree (n `div` 2))
                                                  (arbTree (n `div` 4)))]

          snode x a b = sift (node x a b)

          sift E = E
          sift t@(L _ x a E)
            | a == E || x <= minElem a = t
          sift (L r x (L r' y a b) E) =
                L r y (sift (L r' x a b)) E
          sift t@(L r x a b)
            | x <= minElem a && x <= minElem b = t
          sift (L r x (L r' y a b) c)
            | y <= minElem c =
                L r y (sift (L r' x a b)) c
          sift (L r x a (L r' y b c)) =
                L r y a (sift (L r' x b c))

  coarbitrary E = variant 0
  coarbitrary (L _ x a b) = 
      variant 1 . coarbitrary x . coarbitrary a . coarbitrary b
