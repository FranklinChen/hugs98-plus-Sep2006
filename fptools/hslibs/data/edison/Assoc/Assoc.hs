-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Assoc
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    ( -- associative collections
{-
    -- non-observable classes
    AssocX(..),
    OrdAssocX(..),
    FiniteMapX(..),
    OrdFiniteMapX(..),

    -- observable classes
    Assoc(..),
    OrdAssoc(..),
    FiniteMap(..),
    OrdFiniteMap(..),

    -- specialize sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    lookupList,
    elementsList,
    unsafeFromOrdList,
    fromListWith,
    fromListWithKey,
    insertListWith,
    insertListWithKey,
    unionListWith,
    toList,
    keysList,
    toOrdList,
    unionListWithKey,
-}
    module Assoc,

    -- re-export view types from EdisonPrelude for convenience
    Maybe2(..),
    Maybe3(..)
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)

import EdisonPrelude(Maybe2(..), Maybe3(..))

import Sequence(Sequence)
import ListSeq()

-- class (Eq k, Functor (m k)) => AssocX m k
class Eq k => AssocX m k where
  empty          :: m k a

  single         :: k -> a -> m k a
  fromSeq        :: Sequence seq => seq (k,a) -> m k a

  insert         :: k -> a -> m k a -> m k a
  insertSeq      :: Sequence seq => seq (k,a) -> m k a -> m k a

  union          :: m k a -> m k a -> m k a
  unionSeq       :: Sequence seq => seq (m k a) -> m k a

  delete         :: k -> m k a -> m k a
  deleteAll      :: k -> m k a -> m k a
  deleteSeq      :: Sequence seq => seq k -> m k a -> m k a

  null           :: m k a -> Bool
  size           :: m k a -> Int

  member         :: m k a -> k -> Bool
  count          :: m k a -> k -> Int

  lookup         :: m k a -> k -> a
  lookupM        :: m k a -> k -> Maybe a
  lookupAll      :: Sequence seq => m k a -> k -> seq a
  lookupWithDefault  :: a -> m k a -> k -> a

  adjust         :: (a -> a) -> k -> m k a -> m k a
  adjustAll      :: (a -> a) -> k -> m k a -> m k a

  -- only because can't yet put Functor as superclass
  map            :: (a -> b) -> m k a -> m k b

  fold           :: (a -> b -> b) -> b -> m k a -> b
  fold1          :: (a -> a -> a) -> m k a -> a

  filter         :: (a -> Bool) -> m k a -> m k a
  partition      :: (a -> Bool) -> m k a -> (m k a, m k a)
  
  elements       :: Sequence seq => m k a -> seq a

  instanceName   :: m k a -> String

class (AssocX m k, Ord k) => OrdAssocX m k where
  minView            :: m k a -> Maybe2 a (m k a)
  minElem            :: m k a -> a
  deleteMin          :: m k a -> m k a
  unsafeInsertMin    :: k -> a -> m k a -> m k a

  maxView            :: m k a -> Maybe2 (m k a) a
  maxElem            :: m k a -> a
  deleteMax          :: m k a -> m k a
  unsafeInsertMax    :: m k a -> k -> a -> m k a

  foldr              :: (a -> b -> b) -> b -> m k a -> b
  foldl              :: (b -> a -> b) -> b -> m k a -> b

  foldr1             :: (a -> a -> a) -> m k a -> m k a
  foldl1             :: (a -> a -> a) -> m k a -> m k a

  unsafeFromOrdSeq   :: Sequence seq => seq (k,a) -> m k a
  unsafeAppend       :: m k a -> m k a -> m k a

  filterLT           :: k -> m k a -> m k a
  filterLE           :: k -> m k a -> m k a
  filterGT           :: k -> m k a -> m k a
  filterGE           :: k -> m k a -> m k a

  partitionLT_GE     :: k -> m k a -> (m k a, m k a)
  partitionLE_GT     :: k -> m k a -> (m k a, m k a)
  partitionLT_GT     :: k -> m k a -> (m k a, m k a)

class AssocX m k => FiniteMapX m k where
  fromSeqWith        :: Sequence seq => (a -> a -> a) -> seq (k,a) -> m k a
  fromSeqWithKey     :: Sequence seq => (k -> a -> a -> a) -> seq (k,a) -> m k a

  insertWith         :: (a -> a -> a) -> k -> a -> m k a -> m k a
  insertWithKey      :: (k -> a -> a -> a) -> k -> a -> m k a -> m k a

  insertSeqWith      :: Sequence seq => 
                           (a -> a -> a) -> seq (k,a) -> m k a -> m k a
  insertSeqWithKey   :: Sequence seq => 
                           (k -> a -> a -> a) -> seq (k,a) -> m k a -> m k a

  unionl             :: m k a -> m k a -> m k a
  unionr             :: m k a -> m k a -> m k a
  unionWith          :: (a -> a -> a) -> m k a -> m k a -> m k a

  unionSeqWith       :: Sequence seq => (a -> a -> a) -> seq (m k a) -> m k a

  intersectWith      :: (a -> b -> c) -> m k a -> m k b -> m k c

  difference         :: m k a -> m k b -> m k a

  subset             :: m k a -> m k b -> Bool    
  subsetEq           :: m k a -> m k b -> Bool    

class (OrdAssocX m k, FiniteMapX m k) => OrdFiniteMapX m k
  -- no methods?


class AssocX m k => Assoc m k where
  toSeq             :: Sequence seq => m k a -> seq (k,a)
  keys              :: Sequence seq => m k a -> seq k
  
  mapWithKey        :: (k -> a -> b) -> m k a -> m k b
  foldWithKey       :: (k -> a -> b -> b) -> b -> m k a -> b

  filterWithKey     :: (k -> a -> Bool) -> m k a -> m k a
  partitionWithKey  :: (k -> a -> Bool) -> m k a -> (m k a, m k a)

class (Assoc m k, OrdAssocX m k) => OrdAssoc m k where
  minViewWithKey  :: m k a -> Maybe3 k a (m k a)
  minElemWithKey  :: m k a -> (k,a)

  maxViewWithKey  :: m k a -> Maybe3 (m k a) k a
  maxElemWithKey  :: m k a -> (k,a)

  foldrWithKey    :: (k -> a -> b -> b) -> b -> m k a -> b
  foldlWithKey    :: (b -> k -> a -> b) -> b -> m k a -> b

  toOrdSeq        :: Sequence seq => m k a -> seq (k,a)

class (Assoc m k, FiniteMapX m k) => FiniteMap m k where
  unionWithKey      :: (k -> a -> a -> a) -> m k a -> m k a -> m k a
  unionSeqWithKey   :: Sequence seq => (k -> a -> a -> a) -> seq (m k a) -> m k a

  intersectWithKey  :: (k -> a -> b -> c) -> m k a -> m k b -> m k c

class (OrdAssoc m k, FiniteMap m k) => OrdFiniteMap m k
  -- no methods


-- specialize sequence operations to lists

fromList          :: AssocX m k => [(k,a)] -> m k a
insertList        :: AssocX m k => [(k,a)] -> m k a -> m k a
unionList         :: AssocX m k => [m k a] -> m k a
deleteList        :: AssocX m k => [k] -> m k a -> m k a
lookupList        :: AssocX m k => m k a -> k -> [a]
elementsList      :: AssocX m k => m k a -> [a]
unsafeFromOrdList :: OrdAssocX m k => [(k,a)] -> m k a
fromListWith      :: FiniteMapX m k => (a -> a -> a) -> [(k,a)] -> m k a
fromListWithKey   :: FiniteMapX m k => (k -> a -> a -> a) -> [(k,a)] -> m k a
insertListWith    :: FiniteMapX m k => 
                         (a -> a -> a) -> [(k,a)] -> m k a -> m k a
insertListWithKey :: FiniteMapX m k => 
                         (k -> a -> a -> a) -> [(k,a)] -> m k a -> m k a
unionListWith     :: FiniteMapX m k => (a -> a -> a) -> [m k a] -> m k a
toList            :: Assoc m k => m k a -> [(k,a)]
keysList          :: Assoc m k => m k a -> [k]
toOrdList         :: OrdAssoc m k => m k a -> [(k,a)]
unionListWithKey  :: FiniteMap m k => (k -> a -> a -> a) -> [m k a] -> m k a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
lookupList = lookupAll
elementsList = elements
unsafeFromOrdList = unsafeFromOrdSeq
fromListWith = fromSeqWith
fromListWithKey = fromSeqWithKey
insertListWith = insertSeqWith
insertListWithKey = insertSeqWithKey
unionListWith = unionSeqWith
toList = toSeq
keysList = keys
toOrdList = toOrdSeq
unionListWithKey = unionSeqWithKey


{-
Leave out until somebody asks for:
witness????
compose????

  nub           :: m k a -> m k a  -- ???
  nubWith       :: (a -> a -> a) -> m k a -> m k a
  nubWithKey :: (k -> a -> a -> a) -> m k a -> m k a

  group         :: m k a -> m k [a] -- ???
?????  unsafeMapMonotonim k :: (a -> a) -> m k a -> m k a


-- adjustPartial??? (adjustOrDelete???)
-- adjustAll       :: (a -> a) -> k -> m k a -> m k a
-- unionMap???
-- mapPartial???

  anyViewKey :: m k a -> Maybe3 k a (m k a)
  anyKeyElem :: m k a -> (k,a) -- signals error if collection is empty
  deleteAny :: m k a -> m k a -- could go in AssocX but no point
    -- anyKeyElem and deleteAny must be consistent
    -- do they need to be consistent with anyView?

-- unionMap???
-- mapPartial???

  deleteAllList :: [k] -> m k a -> m k a

  disjoint      :: m k a -> m k b -> Bool

-}

