-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module MinHeap
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- generic adaptor for bags to keep the minimum element separately
    Min, -- instance of Coll/CollX, OrdColl/OrdCollX

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

data Min h a = E | M a (h a)  deriving (Eq)
-- NB: this 'deriving' will only work if we have 
--	-fallow-undecidable-instances
-- The derived instance decl has form
--	instance (Eq a, Eq (h a)) => Eq (Min h a)
-- which needs the undecidable instances flag.

moduleName = "MinHeap"

instanceName E = "MinHeap(empty)"
instanceName (M x h) = "MinHeap(" ++ C.instanceName h ++ ")"

empty     :: Min h a
single    :: (C.CollX h a,Ord a) => a -> Min h a
fromSeq   :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a
insert    :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
insertSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a -> Min h a
union     :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a -> Min h a
unionSeq  :: (C.OrdColl h a,Ord a,S.Sequence s) => s (Min h a) -> Min h a
delete    :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
deleteAll :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
deleteSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a -> Min h a
null      :: Min h a -> Bool
size      :: C.CollX h a => Min h a -> Int
member    :: (C.CollX h a,Ord a) => Min h a -> a -> Bool
count     :: (C.CollX h a,Ord a) => Min h a -> a -> Int

toSeq     :: (C.Coll h a,S.Sequence s) => Min h a -> s a
lookup    :: (C.Coll h a,Ord a) => Min h a -> a -> a
lookupM   :: (C.Coll h a,Ord a) => Min h a -> a -> Maybe a
lookupAll :: (C.Coll h a,Ord a,S.Sequence s) => Min h a -> a -> s a
lookupWithDefault :: (C.Coll h a,Ord a) => a -> Min h a -> a -> a
fold      :: (C.Coll h a) => (a -> b -> b) -> b -> Min h a -> b
fold1     :: (C.Coll h a) => (a -> a -> a) -> Min h a -> a
filter    :: (C.OrdColl h a) => (a -> Bool) -> Min h a -> Min h a
partition :: (C.OrdColl h a) => (a -> Bool) -> Min h a -> (Min h a, Min h a)

deleteMin :: (C.OrdColl h a,Ord a) => Min h a -> Min h a
deleteMax :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a
unsafeInsertMin :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
unsafeInsertMax :: (C.OrdCollX h a,Ord a) => Min h a -> a -> Min h a
unsafeFromOrdSeq :: (C.OrdCollX h a,Ord a,S.Sequence s) => s a -> Min h a
unsafeAppend :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a -> Min h a
filterLT :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
filterLE :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
filterGT :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
filterGE :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
partitionLT_GE :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)
partitionLE_GT :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)
partitionLT_GT :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)

minView :: (C.OrdColl h a,Ord a) => Min h a -> Maybe2 a (Min h a)
minElem :: (C.OrdColl h a,Ord a) => Min h a -> a
maxView :: (C.OrdColl h a,Ord a) => Min h a -> Maybe2 (Min h a) a
maxElem :: (C.OrdColl h a,Ord a) => Min h a -> a
foldr :: (C.OrdColl h a,Ord a) => (a -> b -> b) -> b -> Min h a -> b
foldl :: (C.OrdColl h a,Ord a) => (b -> a -> b) -> b -> Min h a -> b
foldr1 :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
foldl1 :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
toOrdSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => Min h a -> s a

-- export?
fromPrim xs = case C.minView xs of
                Nothing2 -> E
                Just2 x xs' -> M x xs'

-- export?
toPrim E = C.empty
toPrim (M x xs) = C.unsafeInsertMin x xs

empty = E
single x = M x C.empty

fromSeq = fromPrim . C.fromSeq

insert x E = M x C.empty
insert x (M y xs)
  | x <= y    = M x (C.unsafeInsertMin y xs)
  | otherwise = M y (C.insert x xs)

insertSeq xs E = fromSeq xs
insertSeq xs (M y ys) = 
    case C.minView xs_ys of
      Nothing2 -> M y C.empty
      Just2 x rest
          | x < y     -> M x (C.insert y rest)
          | otherwise -> M y xs_ys
  where xs_ys = C.insertSeq xs ys

union E ys = ys
union xs E = xs
union (M x xs) (M y ys)
  | x <= y    = M x (C.union xs (C.unsafeInsertMin y ys))
  | otherwise = M y (C.union (C.unsafeInsertMin x xs) ys)

unionSeq = unionSeqUsingReduce

delete x E = E
delete x m@(M y ys)
  | x > y     = M y (C.delete x ys)
  | x == y    = fromPrim ys
  | otherwise = m

deleteAll x E = E
deleteAll x m@(M y ys)
  | x > y     = M y (C.deleteAll x ys)
  | x == y    = fromPrim (C.deleteAll x ys)
  | otherwise = m

deleteSeq = deleteSeqUsingDelete

null E = True
null (M x xs) = False

size E = 0
size (M x xs) = 1 + C.size xs


member E x = False
member (M y ys) x
  | x > y     = C.member ys x
  | otherwise = (x == y)

count E x = 0
count (M y ys) x
  | x > y     = C.count ys x
  | x == y    = 1 + C.count ys x
  | otherwise = 0

toSeq E = S.empty
toSeq (M x xs) = S.cons x (C.toSeq xs)

lookup (M y ys) x
  | x > y  = C.lookup ys x
  | x == y = y
lookup _ _ = error "MinHeap.lookup: empty heap"

lookupM (M y ys) x
  | x > y  = C.lookupM ys x
  | x == y = Just y
lookupM _ _ = Nothing

lookupAll (M y ys) x
  | x > y  = C.lookupAll ys x
  | x == y = S.cons y (C.lookupAll ys x)
lookupAll _ _ = S.empty

lookupWithDefault d (M y ys) x
  | x > y  = C.lookupWithDefault d ys x
  | x == y = y
lookupWithDefault d _ _ = d

fold f e E = e
fold f e (M x xs) = f x (C.fold f e xs)

fold1 f E = error "MinHeap.fold1: empty heap"
fold1 f (M x xs) = C.fold f x xs

filter p E = E
filter p (M x xs)
  | p x       = M x (C.filter p xs)
  | otherwise = fromPrim (C.filter p xs)

partition p E = (E, E)
partition p (M x xs)
    | p x       = (M x ys, fromPrim zs)
    | otherwise = (fromPrim ys, M x zs)
  where (ys,zs) = C.partition p xs

deleteMin E = E
deleteMin (M x xs) = fromPrim xs

deleteMax E = E
deleteMax (M x xs)
  | C.null xs   = E
  | otherwise = M x (C.deleteMax xs)

unsafeInsertMin x xs = M x (toPrim xs)

unsafeInsertMax E x = M x C.empty
unsafeInsertMax (M y ys) x = M y (C.unsafeInsertMax ys x)

unsafeFromOrdSeq xs =
  case S.lview xs of
    Nothing2   -> E
    Just2 x xs' -> M x (C.unsafeFromOrdSeq xs')

unsafeAppend E ys = ys
unsafeAppend (M x xs) ys = M x (C.unsafeAppend xs (toPrim ys))

filterLT x (M y ys) | y < x  = M y (C.filterLT x ys)
filterLT _ _ = E

filterLE x (M y ys) | y <= x = M y (C.filterLE x ys)
filterLE _ _ = E

filterGT x (M y ys) | y <= x = fromPrim (C.filterGT x ys)
filterGT x h = h

filterGE x (M y ys) | y < x  = fromPrim (C.filterGE x ys)
filterGE x h = h

partitionLT_GE x (M y ys)
  | y < x = (M y lows, fromPrim highs)
  where (lows,highs) = C.partitionLT_GE x ys
partitionLT_GE x h = (E, h)

partitionLE_GT x (M y ys) 
  | y <= x = (M y lows, fromPrim highs)
  where (lows,highs) = C.partitionLE_GT x ys
partitionLE_GT x h = (E, h)

partitionLT_GT x (M y ys)
  | y < x  = let (lows,highs) = C.partitionLT_GT x ys
             in (M y lows, fromPrim highs)
  | y == x = (E, fromPrim (C.filterGT x ys))
partitionLT_GT x h = (E, h)


minView E = Nothing2
minView (M x xs) = Just2 x (fromPrim xs)

minElem E = error "MinHeap.minElem: empty heap"
minElem (M x xs) = x

maxView E = Nothing2
maxView (M x xs) = case C.maxView xs of
                     Nothing2   -> Just2 E x
                     Just2 ys y -> Just2 (M x ys) y

maxElem E = error "MinHeap.minElem: empty heap"
maxElem (M x xs)
  | C.null xs   = x
  | otherwise = C.maxElem xs

foldr f e E = e
foldr f e (M x xs) = f x (C.foldr f e xs)

foldl f e E = e
foldl f e (M x xs) = C.foldl f (f e x) xs

foldr1 f E = error "MinHeap.foldr1: empty heap"
foldr1 f (M x xs)
  | C.null xs   = x
  | otherwise = f x (C.foldr1 f xs)

foldl1 f E = error "MinHeap.foldl1: empty heap"
foldl1 f (M x xs) = C.foldl f x xs

toOrdSeq E = S.empty
toOrdSeq (M x xs) = S.cons x (C.toOrdSeq xs)


-- instance declarations

instance (C.OrdColl h a, Ord a) => C.CollX (Min h) a where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   instanceName c = moduleName}

instance (C.OrdColl h a, Ord a) => C.OrdCollX (Min h) a where
  {deleteMin = deleteMin; deleteMax = deleteMax; 
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax; 
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend; 
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT; 
   filterGE = filterGE; partitionLT_GE = partitionLT_GE; 
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance (C.OrdColl h a, Ord a) => C.Coll (Min h) a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM; 
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault; 
   fold = fold; fold1 = fold1; filter = filter; partition = partition}

instance (C.OrdColl h a, Ord a) => C.OrdColl (Min h) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldl = foldl; foldr1 = foldr1; 
   foldl1 = foldl1; toOrdSeq = toOrdSeq}

-- instance Eq is derived

instance (C.OrdColl h a, Show a) => Show (Min h a) where
  show xs = show (C.toOrdList xs)

instance (C.OrdColl h a,Arbitrary (h a),Arbitrary a) => Arbitrary (Min h a) where
  arbitrary = do xs <- arbitrary
                 x  <- arbitrary
                 i  <- arbitrary :: Gen Int
                 return (if C.null xs || x <= C.minElem xs then M x xs
                         else if odd i then M (C.minElem xs) xs
                                       else fromPrim xs)

  coarbitrary E = variant 0
  coarbitrary (M x xs) = variant 1 . coarbitrary x . coarbitrary xs


