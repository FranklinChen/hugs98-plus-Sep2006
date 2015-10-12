-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module CollectionDefaults
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Collection
import qualified Sequence as S
import qualified ListSeq as L

insertSeqUsingUnion :: (CollX c a,S.Sequence seq) => seq a -> c a -> c a
insertSeqUsingUnion xs c = union (fromSeq xs) c

insertSeqUsingFoldr :: (CollX c a,S.Sequence seq) => seq a -> c a -> c a
insertSeqUsingFoldr xs c = S.foldr insert c xs

memberUsingFold :: Coll c a => c a -> a -> Bool
memberUsingFold h x = fold (\y ans -> (x == y) || ans) False h

countUsingMember :: SetX c a => c a -> a -> Int
countUsingMember xs x = if member xs x then 1 else 0

lookupAllUsingLookupM :: (Set c a,S.Sequence seq) => c a -> a -> seq a
lookupAllUsingLookupM xs x =
  case lookupM xs x of
    Nothing -> S.empty
    Just y -> S.single y

deleteSeqUsingDelete :: (CollX c a,S.Sequence seq) => seq a -> c a -> c a
deleteSeqUsingDelete xs c = S.foldr delete c xs

unionSeqUsingFoldl :: (CollX c a,S.Sequence seq) => seq (c a) -> c a
unionSeqUsingFoldl = S.foldl union empty

unionSeqUsingReduce :: (CollX c a,S.Sequence seq) => seq (c a) -> c a
unionSeqUsingReduce = S.reducel union empty

fromSeqUsingFoldr :: (CollX c a,S.Sequence seq) => seq a -> c a
fromSeqUsingFoldr = S.foldr insert empty

fromSeqUsingUnionSeq :: (CollX c a,S.Sequence seq) => seq a -> c a
fromSeqUsingUnionSeq = unionList . S.foldl singleCons []
  where singleCons xs x = S.cons (single x) xs

toSeqUsingFold :: (Coll c a,S.Sequence seq) => c a -> seq a
toSeqUsingFold = fold S.cons S.empty

unsafeInsertMaxUsingUnsafeAppend :: OrdCollX c a => c a -> a -> c a
unsafeInsertMaxUsingUnsafeAppend c x = unsafeAppend c (single x)

toOrdSeqUsingFoldr :: (OrdColl c a,S.Sequence seq) => c a -> seq a
toOrdSeqUsingFoldr = foldr S.cons S.empty

unsafeFromOrdSeqUsingUnsafeInsertMin :: 
    (OrdCollX c a,S.Sequence seq) => seq a -> c a
unsafeFromOrdSeqUsingUnsafeInsertMin = S.foldr unsafeInsertMin empty

disjointUsingToOrdList :: OrdColl c a => c a -> c a -> Bool
disjointUsingToOrdList xs ys = disj (toOrdList xs) (toOrdList ys)
  where disj a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> disj xs b
            EQ -> False
            GT -> disj a ys
        disj _ _ = True

intersectWitnessUsingToOrdList :: OrdColl c a => c a -> c a -> Maybe2 a a
intersectWitnessUsingToOrdList xs ys = witness (toOrdList xs) (toOrdList ys)
  where witness a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> witness xs b
            EQ -> Just2 x y
            GT -> witness a ys
        witness _ _ = Nothing2

lookupUsingLookupM :: Coll c a => c a -> a -> a
lookupUsingLookupM ys x =
  case lookupM ys x of
    Just y -> y
    Nothing -> error (instanceName ys ++ ".lookup: lookup failed")


lookupUsingLookupAll :: Coll c a => c a -> a -> a
lookupUsingLookupAll ys x =
  case lookupAll ys x of
    (y:_) -> y
    [] -> error (instanceName ys ++ ".lookup: lookup failed")

lookupMUsingLookupAll :: Coll c a => c a -> a -> Maybe a
lookupMUsingLookupAll ys x =
  case lookupAll ys x of
    (y:_) -> Just y
    [] -> Nothing

lookupWithDefaultUsingLookupAll :: Coll c a => a -> c a -> a -> a
lookupWithDefaultUsingLookupAll dflt ys x =
  case lookupAll ys x of
    (y:_) -> y
    [] -> dflt

lookupWithDefaultUsingLookupM :: Coll c a => a -> c a -> a -> a
lookupWithDefaultUsingLookupM dflt ys x =
  case lookupM ys x of
    Just y -> y
    Nothing -> dflt

deleteMaxUsingMaxView :: OrdColl c a => c a -> c a
deleteMaxUsingMaxView c =
  case maxView c of
    Just2 c' _ -> c'
    Nothing2 -> c

fromSeqWithUsingInsertWith :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq a -> c a
fromSeqWithUsingInsertWith c = S.foldr (insertWith c) empty

insertUsingInsertWith :: Set c a => a -> c a -> c a
insertUsingInsertWith = insertWith (\x y -> x)

unionUsingUnionWith :: Set c a => c a -> c a -> c a
unionUsingUnionWith = unionWith (\x y -> x)

filterUsingOrdLists :: OrdColl c a => (a -> Bool) -> c a -> c a
filterUsingOrdLists p = unsafeFromOrdList . L.filter p . toOrdList

partitionUsingOrdLists :: OrdColl c a => (a -> Bool) -> c a -> (c a,c a)
partitionUsingOrdLists p xs = (unsafeFromOrdList ys,unsafeFromOrdList zs)
  where (ys,zs) = L.partition p (toOrdList xs)

intersectUsingIntersectWith :: Set c a => c a -> c a -> c a
intersectUsingIntersectWith = intersectWith (\x y -> x)

differenceUsingOrdLists :: OrdSet c a => c a -> c a -> c a
differenceUsingOrdLists xs ys = unsafeFromOrdList (diff (toOrdList xs) (toOrdList ys))
  where diff a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : diff xs b
            EQ -> diff xs ys
            GT -> diff a ys
        diff a _ = a

subsetUsingOrdLists :: OrdSet c a => c a -> c a -> Bool
subsetUsingOrdLists xs ys = subsetOnOrdLists (toOrdList xs) (toOrdList ys)

subsetEqUsingOrdLists :: OrdSet c a => c a -> c a -> Bool
subsetEqUsingOrdLists xs ys = subsetEqOnOrdLists (toOrdList xs) (toOrdList ys)

subsetOnOrdLists [] [] = False
subsetOnOrdLists [] (_:_) = True
subsetOnOrdLists (_:_) [] = False
subsetOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> subsetOnOrdLists xs ys
    GT -> subsetEqOnOrdLists a ys

subsetEqOnOrdLists [] _ = True
subsetEqOnOrdLists (_:_) [] = False
subsetEqOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> subsetEqOnOrdLists xs ys
    GT -> subsetEqOnOrdLists a ys

insertSeqWithUsingInsertWith :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq a -> c a -> c a
insertSeqWithUsingInsertWith c xs s = S.foldr (insertWith c) s xs

unionlUsingUnionWith :: Set c a => c a -> c a -> c a
unionlUsingUnionWith xs ys = unionWith (\x y -> x) xs ys

unionrUsingUnionWith :: Set c a => c a -> c a -> c a
unionrUsingUnionWith xs ys = unionWith (\x y -> y) xs ys

unionWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c a -> c a -> c a
unionWithUsingOrdLists c xs ys = unsafeFromOrdList (merge (toOrdList xs) (toOrdList ys))
  where merge a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : merge xs b
            EQ -> c x y : merge xs ys
            GT -> y : merge a ys
        merge a@(x:xs) [] = a
        merge [] b = b

unionSeqWithUsingReducer :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq (c a) -> c a
unionSeqWithUsingReducer c = S.reducer (unionWith c) empty

intersectWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c a -> c a -> c a
intersectWithUsingOrdLists c xs ys = unsafeFromOrdList (inter (toOrdList xs) (toOrdList ys))
  where inter a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> inter xs b
            EQ -> c x y : inter xs ys
            GT -> inter a ys
        inter _ _ = []




