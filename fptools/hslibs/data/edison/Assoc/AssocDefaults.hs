-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module AssocDefaults 
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import Assoc
import qualified Sequence as S
-- import qualified ListSeq as L

fromSeqUsingInsertSeq :: 
    (AssocX m k,S.Sequence seq) => seq (k,a) -> m k a
fromSeqUsingInsertSeq kvs = insertSeq kvs empty

insertSeqUsingFoldr :: 
    (AssocX m k,S.Sequence seq) => seq (k,a) -> m k a -> m k a
insertSeqUsingFoldr kvs m = S.foldr (uncurry insert) m kvs

unionSeqUsingReduce :: (AssocX m k,S.Sequence seq) => seq (m k a) -> m k a
unionSeqUsingReduce ms = S.reducel union empty ms

deleteSeqUsingFoldr :: (AssocX m k,S.Sequence seq) => seq k -> m k a -> m k a
deleteSeqUsingFoldr ks m = S.foldr delete m ks

countUsingMember :: AssocX m k => m k a -> k -> Int
countUsingMember m k = if member m k then 1 else 0

lookupAllUsingLookupM :: (AssocX m k,S.Sequence seq) => m k a -> k -> seq a
lookupAllUsingLookupM m k = case lookupM m k of
                              Just x -> S.single x
                              Nothing -> S.empty

lookupWithDefaultUsingLookupM :: AssocX m k => a -> m k a -> k -> a
lookupWithDefaultUsingLookupM d m k = case lookupM m k of
                                        Just x -> x
                                        Nothing -> d

partitionUsingFilter :: AssocX m k => (a -> Bool) -> m k a -> (m k a,m k a)
partitionUsingFilter f m = (filter f m, filter (not . f) m)

elementsUsingFold :: (AssocX m k,S.Sequence seq) => m k a -> seq a
elementsUsingFold = fold S.cons S.empty

insertWithUsingLookupM :: 
    FiniteMapX m k => (a -> a -> a) -> k -> a -> m k a -> m k a
insertWithUsingLookupM f k x m =
    case lookupM m k of
      Nothing -> insert k x m
      Just y -> insert k (f x y) m

fromSeqWithUsingInsertSeqWith ::
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (k,a) -> m k a
fromSeqWithUsingInsertSeqWith f kvs = insertSeqWith f kvs empty

fromSeqWithKeyUsingInsertSeqWithKey :: 
    (FiniteMapX m k,S.Sequence seq) => (k -> a -> a -> a) -> seq (k,a) -> m k a
fromSeqWithKeyUsingInsertSeqWithKey f kvs = insertSeqWithKey f kvs empty

insertWithKeyUsingInsertWith :: 
    FiniteMapX m k => (k -> a -> a -> a) -> k -> a -> m k a -> m k a
insertWithKeyUsingInsertWith f k = insertWith (f k) k

insertSeqWithUsingInsertWith :: 
    (FiniteMapX m k,S.Sequence seq) => 
      (a -> a -> a) -> seq (k,a) -> m k a -> m k a
insertSeqWithUsingInsertWith f kvs m =
    S.foldr (uncurry (insertWith f)) m kvs

insertSeqWithKeyUsingInsertWithKey ::
    (FiniteMapX m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (k,a) -> m k a -> m k a
insertSeqWithKeyUsingInsertWithKey f kvs m =
    S.foldr (uncurry (insertWithKey f)) m kvs

unionSeqWithUsingReduce :: 
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (m k a) -> m k a
unionSeqWithUsingReduce f ms = S.reducel (unionWith f) empty ms

unionSeqWithUsingFoldr :: 
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (m k a) -> m k a
unionSeqWithUsingFoldr f ms = S.foldr (unionWith f) empty ms

toSeqUsingFoldWithKey :: (Assoc m k,S.Sequence seq) => m k a -> seq (k,a)
toSeqUsingFoldWithKey = foldWithKey conspair S.empty
  where conspair k v kvs = S.cons (k,v) kvs

keysUsingFoldWithKey :: (Assoc m k,S.Sequence seq) => m k a -> seq k
keysUsingFoldWithKey = foldWithKey conskey S.empty
  where conskey k v ks = S.cons k ks

unionWithUsingInsertWith :: 
    FiniteMap m k => (a -> a -> a) -> m k a -> m k a -> m k a
unionWithUsingInsertWith f m1 m2 = foldWithKey (insertWith f) m2 m1

unionWithKeyUsingInsertWithKey :: 
    FiniteMap m k => (k -> a -> a -> a) -> m k a -> m k a -> m k a
unionWithKeyUsingInsertWithKey f m1 m2 = foldWithKey (insertWithKey f) m2 m1

unionSeqWithKeyUsingReduce :: 
    (FiniteMap m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (m k a) -> m k a
unionSeqWithKeyUsingReduce f ms = S.reducel (unionWithKey f) empty ms

unionSeqWithKeyUsingFoldr :: 
    (FiniteMap m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (m k a) -> m k a
unionSeqWithKeyUsingFoldr f ms = S.foldr (unionWithKey f) empty ms

intersectWithUsingLookupM :: 
    FiniteMap m k => (a -> b -> c) -> m k a -> m k b -> m k c
intersectWithUsingLookupM f m1 m2 = foldWithKey ins empty m1
  where ins k x m = case lookupM m2 k of
                      Nothing -> m
                      Just y -> insert k (f x y) m

intersectWithKeyUsingLookupM :: 
    FiniteMap m k => (k -> a -> b -> c) -> m k a -> m k b -> m k c
intersectWithKeyUsingLookupM f m1 m2 = foldWithKey ins empty m1
  where ins k x m = case lookupM m2 k of
                      Nothing -> m
                      Just y -> insert k (f k x y) m

differenceUsingDelete :: FiniteMap m k => m k a -> m k b -> m k a
differenceUsingDelete m1 m2 = foldWithKey del m1 m2
  where del k _ m = delete k m

subsetUsingSubsetEq :: FiniteMapX m k => m k a -> m k b -> Bool
subsetUsingSubsetEq m1 m2 = subsetEq m1 m2 && size m1 < size m2

subsetEqUsingMember :: FiniteMap m k => m k a -> m k b -> Bool
subsetEqUsingMember m1 m2 = foldWithKey mem True m1
  where mem k _ b = member m2 k && b
