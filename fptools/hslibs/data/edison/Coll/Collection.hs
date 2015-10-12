-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Collection
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
{-
    -- non-observable classes
    CollX(..),
    OrdCollX(..),
    SetX(..),
    OrdSetX(..),

    -- observable classes
    Coll(..),
    OrdColl(..),
    Set(..),
    OrdSet(..),

    -- specialize all the sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    unsafeFromOrdList,
    toList,
    lookupList,
    toOrdList,
    fromListWith,
    insertListWith,
    unionListWith,
-}
    module Collection,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(..)
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import EdisonPrelude(Maybe2(..))
import Sequence(Sequence)
import ListSeq()

class Eq a => CollX c a where
  empty          :: c a

    -- the empty collection

  single         :: a -> c a

    -- create a singleton collection

  fromSeq        :: Sequence seq => seq a -> c a

    -- convert a sequence to a collection.  For sets, it is unspecified
    -- which element is kept in case of duplicates.

  insert         :: a -> c a -> c a
  insertSeq      :: Sequence seq => seq a -> c a -> c a

    -- insert an element or a sequence of elements into a collection.  For
    -- sets, insert keeps the new element in case of duplicates, but
    -- insertSeq keeps an unspecified element.

  union          :: c a -> c a -> c a
  unionSeq       :: Sequence seq => seq (c a) -> c a

    -- merge two collections or a sequence of collections.  For sets, it
    -- is unspecified which element is kept in case of duplicates.

  delete         :: a -> c a -> c a
  deleteAll      :: a -> c a -> c a

    -- delete a single occurrence or all occurrences of the given
    -- element from a collection.  For sets, these will be the same,
    -- but for bags they may be different.  For delete on bags, it
    -- is unspecified which of several duplicate elements is deleted.

  deleteSeq      :: Sequence seq => seq a -> c a -> c a

    -- delete a single occurrence of each of the given elements from
    -- a collection.  For bags, there may be multiple occurrences of a
    -- given element in the collection, in which case it is unspecified
    -- which is deleted.

  null           :: c a -> Bool
  size           :: c a -> Int

    -- test whether the collection is empty, or return the number of
    -- elements in the collection.

  member         :: c a -> a -> Bool
  count          :: c a -> a -> Int

    -- test whether the given element is in the collection, or how many
    -- duplicates are in the collection.  (For sets, count will always
    -- return 0 or 1.)

  instanceName   :: c a -> String

    -- the name of the module implementing c

class (CollX c a, Ord a) => OrdCollX c a where

  deleteMin          :: c a -> c a
  deleteMax          :: c a -> c a

    -- delete the minimum or maximum element from the collection.
    -- If there is more than one minimum or maximum, it is unspecified which
    -- is deleted.

  unsafeInsertMin    :: a -> c a -> c a
  unsafeInsertMax    :: c a -> a -> c a

    -- insert an element that is guaranteed to be <= or >= any existing
    -- elements in the collection.  (For sets, this precondition is
    -- strengthened to < or >.)

  unsafeFromOrdSeq   :: Sequence seq => seq a -> c a

    -- convert a sequence in non-decreasing order into a collection.
    -- (For sets, the sequence must be in increasing order.)

  unsafeAppend       :: c a -> c a -> c a

    -- union two collections where every element in the first
    -- collection is <= every element in the second collection.
    -- (For sets, this precondition is strengthened to <.)

  filterLT           :: a -> c a -> c a
  filterLE           :: a -> c a -> c a
  filterGT           :: a -> c a -> c a
  filterGE           :: a -> c a -> c a

    -- filterLT x xs = filter (< x) xs
    -- filterLE x xs = filter (<= x) xs
    -- filterGT x xs = filter (> x) xs
    -- filterGE x xs = filter (>= x) xs

  partitionLT_GE     :: a -> c a -> (c a, c a)
  partitionLE_GT     :: a -> c a -> (c a, c a)
  partitionLT_GT     :: a -> c a -> (c a, c a)

    -- partitionLT_GE x xs = partition (< x) xs
    -- partitionLE_GT x xs = partition (<= x) xs
    -- partitionLT_GT x xs = (filterLT x xs, filterGT x xs)

class CollX c a => SetX c a where

  intersect   :: c a -> c a -> c a
  difference  :: c a -> c a -> c a

    -- return the intersection or difference of two sets.  For intersect,
    -- it is unspecified which of the two elements is kept.

  subset      :: c a -> c a -> Bool    
  subsetEq    :: c a -> c a -> Bool

    -- test whether the first set is a proper subset of the second,
    -- or whether it is a (possibly improper) subset.

class (OrdCollX c a, SetX c a) => OrdSetX c a
  -- no methods


class CollX c a => Coll c a where
  toSeq      :: Sequence seq => c a -> seq a

    -- list the elements of the collection in an unspecified order

  lookup     :: c a -> a -> a
  lookupM    :: c a -> a -> Maybe a
  lookupAll  :: Sequence seq => c a -> a -> seq a
  lookupWithDefault  :: a -> c a -> a -> a

    -- lookup one or more elements equal to the given element.
    -- if there is none, then lookup signals an error, lookupM returns 
    -- Nothing, lookupAll returns empty, and lookupWithDefault d returns d.
    -- if there are mulitiple copies, then lookup/lookupM/lookupWithDefault
    -- return an unspecified one, and lookupAll returns them all, but
    -- in an unspecified order.

  fold       :: (a -> b -> b) -> b -> c a -> b
  fold1      :: (a -> a -> a) -> c a -> a

    -- fold over all the elements in a collection in unspecified order.
    -- (fold1 signals an error if the collection is empty.)

  filter     :: (a -> Bool) -> c a -> c a
  partition  :: (a -> Bool) -> c a -> (c a, c a)

    -- filter removes all elements not satisfying the predicate.
    -- partition returns two collections, one containing all the
    -- elements satisfying the predicate, and one containing all the
    -- elements not satisfying the predicate.

class (Coll c a, OrdCollX c a) => OrdColl c a where

  minView    :: c a -> Maybe2 a (c a)
  minElem    :: c a -> a

    -- return the minimum element in the collection, together with
    -- the collection without that element in the case of minView.
    -- If there are multiple copies of the minimum element, it is
    -- unspecified which is chosen.  Note that minView, minElem, and
    -- deleteMin may make different choices!

  maxView    :: c a -> Maybe2 (c a) a
  maxElem    :: c a -> a

    -- return the maximum element in the collection, together with
    -- the collection without that element in the case of maxView.
    -- If there are multiple copies of the maximum element, it is
    -- unspecified which is chosen.  Note that maxView, maxElem, and
    -- deleteMax may make different choices!

  foldr      :: (a -> b -> b) -> b -> c a -> b
  foldl      :: (b -> a -> b) -> b -> c a -> b

    -- fold across the elements in non-decreasing order.
    -- (For sets, this will always be increasing order.)

  foldr1     :: (a -> a -> a) -> c a -> a
  foldl1     :: (a -> a -> a) -> c a -> a

    -- fold across the elements in non-decreasing order, or signal an
    -- error if the collection is empty.  (For sets, this will always be 
    -- increasing order.)

  toOrdSeq   :: Sequence seq => c a -> seq a

    -- list the elements in non-decreasing order.

class (Coll c a, SetX c a) => Set c a where

  -- WARNING: Each of the following "With" functions is unsafe.  The combining
  -- functions are required to satisfy the precondition that, given two
  -- equal elements, they return a third element equal to the other two.

  fromSeqWith     :: Sequence seq => (a -> a -> a) -> seq a -> c a

    -- same as fromSeq but with a combining function to resolve duplicates.
    -- Usually, the combining function should be associative.  If not,
    -- the elements will be combined left-to-right, but with an
    -- unspecified associativity.  For example, if x == y == z,
    -- then fromSeqWith (+) [x,y,z] equals either
    --     single (x + (y + z))
    -- or
    --     single ((x + y) + z)

  insertWith      :: (a -> a -> a) -> a -> c a -> c a
  insertSeqWith   :: Sequence seq => (a -> a -> a) -> seq a -> c a -> c a

    -- same as insert/insertSeq but with a combining function to resolve 
    -- duplicates.  The comments about associativity apply to insertSeqWith.

  unionl          :: c a -> c a -> c a
  unionr          :: c a -> c a -> c a

    -- unionl = unionWith (\x y -> x)
    -- unionr = unionWith (\x y -> y)
    
  unionWith       :: (a -> a -> a) -> c a -> c a -> c a
  unionSeqWith    :: Sequence seq => (a -> a -> a) -> seq (c a) -> c a

    -- same as union/unionSeq but with a combining function to resolve
    -- duplicates.  The comments about associativity apply to unionSeqWith.

  intersectWith   :: (a -> a -> a) -> c a -> c a -> c a

    -- same as intersect but with a combining function to resolve duplicates.

class (OrdColl c a, Set c a) => OrdSet c a
  -- no methods


-- specialize all the sequence operations to lists

fromList          :: CollX c a => [a] -> c a
insertList        :: CollX c a => [a] -> c a -> c a
unionList         :: CollX c a => [c a] -> c a
deleteList        :: CollX c a => [a] -> c a -> c a
unsafeFromOrdList :: OrdCollX c a => [a] -> c a
toList            :: Coll c a => c a -> [a]
lookupList        :: Coll c a => c a -> a -> [a]
toOrdList         :: OrdColl c a => c a -> [a]
fromListWith      :: Set c a => (a -> a -> a) -> [a] -> c a
insertListWith    :: Set c a => (a -> a -> a) -> [a] -> c a -> c a
unionListWith     :: Set c a => (a -> a -> a) -> [c a] -> c a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
unsafeFromOrdList = unsafeFromOrdSeq
toList = toSeq
lookupList = lookupAll
toOrdList = toOrdSeq
fromListWith = fromSeqWith
insertListWith = insertSeqWith
unionListWith = unionSeqWith

