-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module PatriciaLoMap
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
    (
    -- type of little-endian Patricia trees
    FM, -- instance of Assoc(X), FiniteMap(X)
        -- also instance of Functor

    -- AssocX operations
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupWithDefault,adjust,adjustAll,map,fold,fold1,filter,partition,elements,

    -- Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,filterWithKey,partitionWithKey,

    -- FiniteMapX operations
    fromSeqWith,fromSeqWithKey,insertWith,insertWithKey,insertSeqWith,
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectWith,
    difference,subset,subsetEq,

    -- FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectWithKey,

    -- documentation
    moduleName,

    -- re-export view types from EdisonPrelude for convenience
    Maybe2(..)
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import EdisonPrelude(Maybe2(..))
import qualified Assoc as A ( AssocX(..), Assoc(..), FiniteMapX(..), FiniteMap(..) )
import qualified Sequence as S
import AssocDefaults
import Data.Int
import Data.Bits

moduleName = "PatriciaLoMap"

-- Adapted from
--   Chris Okasaki and Any Gill.  "Fast Mergeable Integer Maps".
--   Workshop on ML, September 1998, pages 77-86.

data FM k a = -- k is *always* Int
    E
  | L Int a
  | B Int Int !(FM k a) !(FM k a)

type IntMap = FM Int

-- auxiliary functions

makeB p m E t = t
makeB p m t E = t
makeB p m t0 t1 = B p m t0 t1

lmakeB p m E t = t
lmakeB p m t0 t1 = B p m t0 t1

rmakeB p m t E = t
rmakeB p m t0 t1 = B p m t0 t1

lowestBit :: Int32 -> Int32
lowestBit x = x .&. (-x)

branchingBit :: Int -> Int -> Int
branchingBit p0 p1 = 
  fromIntegral (lowestBit (fromIntegral p0 `xor` fromIntegral p1))

mask :: Int -> Int -> Int
mask p m = fromIntegral (fromIntegral p .&. (fromIntegral m - (1 :: Int32)))

zeroBit :: Int -> Int -> Bool
zeroBit p m = (fromIntegral p) .&. (fromIntegral m) == (0 :: Int32)

matchPrefix :: Int -> Int -> Int -> Bool
matchPrefix k p m = mask k m == p

join p0 t0 p1 t1 =
  let m = branchingBit p0 p1
  in if zeroBit p0 m then B (mask p0 m) m t0 t1
                     else B (mask p0 m) m t1 t0

keepR x y = y

-- end auxiliary functions

empty :: IntMap a
empty = E

single :: Int -> a -> IntMap a
single k x = L k x

fromSeq :: S.Sequence seq => seq (Int,a) -> IntMap a
fromSeq = S.foldl (\t (k, x) -> insert k x t) E

insert :: Int -> a -> IntMap a -> IntMap a
insert k x E = L k x
insert k x t@(L j y) = if j == k then L k x else join k (L k x) j t
insert k x t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x t0) t1
                     else B p m t0 (insert k x t1)
    else join k (L k x) p t

union :: IntMap a -> IntMap a -> IntMap a
union s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then B p m (union s0 t) s1
                                 else B p m s0 (union s1 t)
                else join p s q t
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then B q n (union s t0) t1
                                 else B q n t0 (union s t1)
                else join p s q t
  | otherwise = if p == q then B p m (union s0 t0) (union s1 t1)
                else join p s q t
union s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x s0) s1
                     else B p m s0 (insert k x s1)
    else join k (L k x) p s
union s@(B _ _ _ _) E = s
union (L k x) t = insert k x t
union E t = t

delete :: Int -> IntMap a -> IntMap a
delete k E = E
delete k t@(L j x) = if k == j then E else t
delete k t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then lmakeB p m (delete k t0) t1
                     else rmakeB p m t0 (delete k t1)
    else t

null :: IntMap a -> Bool
null E = True
null _ = False

size :: IntMap a -> Int
size E = 0
size (L _ _) = 1
size (B _ _ t0 t1) = size t0 + size t1

member :: IntMap a -> Int -> Bool
member E k = False
member (L j x) k = (j == k)
member (B p m t0 t1) k = if zeroBit k m then member t0 k else member t1 k

lookup :: IntMap a -> Int -> a
lookup E k = error "PatriciaLoMap.lookup: lookup failed"
lookup (L j x) k = if j == k then x else error "PatriciaLoMap.lookup: lookup failed"
lookup (B p m t0 t1) k = if zeroBit k m then lookup t0 k else lookup t1 k

lookupM :: IntMap a -> Int -> Maybe a
lookupM E k = Nothing
lookupM (L j x) k = if j == k then Just x else Nothing
lookupM (B p m t0 t1) k = if zeroBit k m then lookupM t0 k else lookupM t1 k

adjust :: (a -> a) -> Int -> IntMap a -> IntMap a
adjust f k E = E
adjust f k t@(L j x) = if k == j then L k (f x) else t
adjust f k t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (adjust f k t0) t1
                     else B p m t0 (adjust f k t1)
    else t

map :: (a -> b) -> IntMap a -> IntMap b
map f E = E
map f (L k x) = L k (f x)
map f (B p m t0 t1) = B p m (map f t0) (map f t1)

fold :: (a -> b -> b) -> b -> IntMap a -> b
fold f c E = c
fold f c (L k x) = f x c
fold f c (B p m t0 t1) = fold f (fold f c t1) t0

fold1 :: (a -> a -> a) -> IntMap a -> a
fold1 f E = error "PatriciaLoMap.fold1: empty map"
fold1 f (L k x) = x
fold1 f (B p m t0 t1) = f (fold1 f t0) (fold1 f t1)

filter :: (a -> Bool) -> IntMap a -> IntMap a
filter g E = E
filter g t@(L k x) = if g x then t else E
filter g (B p m t0 t1) = makeB p m (filter g t0) (filter g t1)

partition :: (a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partition g E = (E, E)
partition g t@(L k x) = if g x then (t, E) else (E, t)
partition g (B p m t0 t1) =
  let (t0',t0'') = partition g t0
      (t1',t1'') = partition g t1
  in (makeB p m t0' t1', makeB p m t0'' t1'')
  
fromSeqWith :: S.Sequence seq => (a -> a -> a) -> seq (Int,a) -> IntMap a
fromSeqWith f = S.foldl (\t (k, x) -> insertWith f k x t) E

insertWith :: (a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWith f k x E = L k x
insertWith f k x t@(L j y) = if j == k then L k (f x y) else join k (L k x) j t
insertWith f k x t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith f k x t0) t1
                     else B p m t0 (insertWith f k x t1)
    else join k (L k x) p t

unionl :: IntMap a -> IntMap a -> IntMap a
unionl s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionl s0 t) s1
                                 else B p m s0 (unionl s1 t)
                else join p s q t
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionl s t0) t1
                                 else B q n t0 (unionl s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionl s0 t0) (unionl s1 t1)
                else join p s q t
unionl s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith keepR k x s0) s1
                     else B p m s0 (insertWith keepR k x s1)
    else join k (L k x) p s
unionl s@(B _ _ _ _) E = s
unionl (L k x) t = insert k x t
unionl E t = t

unionr :: IntMap a -> IntMap a -> IntMap a
unionr s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionr s0 t) s1
                                 else B p m s0 (unionr s1 t)
                else join p s q t
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionr s t0) t1
                                 else B q n t0 (unionr s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionr s0 t0) (unionr s1 t1)
                else join p s q t
unionr s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x s0) s1
                     else B p m s0 (insert k x s1)
    else join k (L k x) p s
unionr s@(B _ _ _ _) E = s
unionr (L k x) t = insertWith keepR k x t
unionr E t = t

unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWith f s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionWith f s0 t) s1
                                 else B p m s0 (unionWith f s1 t)
                else join p s q t
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionWith f s t0) t1
                                 else B q n t0 (unionWith f s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionWith f s0 t0) (unionWith f s1 t1)
                else join p s q t
unionWith f s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith (flip f) k x s0) s1
                     else B p m s0 (insertWith (flip f) k x s1)
    else join k (L k x) p s
unionWith f s@(B _ _ _ _) E = s
unionWith f (L k x) t = insertWith f k x t
unionWith f E t = t

intersectWith :: (a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectWith f s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then intersectWith f s0 t
                                 else intersectWith f s1 t
                else E
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then intersectWith f s t0
                                 else intersectWith f s t1
                else E
  | otherwise = if p /= q then E
                else makeB p m (intersectWith f s0 t0) (intersectWith f s1 t1)
intersectWith f (B p m s0 s1) (L k y) =
    case lookupM (if zeroBit k m then s0 else s1) k of
      Just x -> L k (f x y)
      Nothing -> E
intersectWith f s@(B _ _ _ _) E = E
intersectWith f (L k x) t =
    case lookupM t k of
      Just y -> L k (f x y)
      Nothing -> E
intersectWith f E t = E

difference :: IntMap a -> IntMap b -> IntMap a
difference s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then lmakeB p m (difference s0 t) s1
                                 else rmakeB p m s0 (difference s1 t)
                else s
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then difference s t0
                                 else difference s t1
                else s
  | otherwise = if p /= q then s
                else makeB p m (difference s0 t0) (difference s1 t1)
difference s@(B p m s0 s1) (L k y) =
    if matchPrefix k p m then
      if zeroBit k m then lmakeB p m (delete k s0) s1
                     else rmakeB p m s0 (delete k s1)
    else s
difference s@(B _ _ _ _) E = s
difference s@(L k x) t = if member t k then E else s
difference E t = E

subset :: IntMap a -> IntMap b -> Bool
subset s t = case subset' s t of {LT -> True; _ -> False}

subset' s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = GT
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then subset' s t0
                                 else subset' s t1
                else GT
  | otherwise = if p == q then case (subset' s0 t0,subset' s1 t1) of
                                  (GT,_)  -> GT
                                  (_,GT)  -> GT
                                  (EQ,EQ) -> EQ
                                  (_,_)   -> LT
                else GT
subset' (B p m s0 s1) _ = GT
subset' (L k x) (L j y) = if k == j then EQ else GT
subset' (L k x) t = if member t k then LT else GT
subset' E E = EQ
subset' E _ = LT

subsetEq :: IntMap a -> IntMap b -> Bool
subsetEq s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = False
  | m > n    = matchPrefix p q n && (if zeroBit p n then subsetEq s t0
                                                     else subsetEq s t1)
  | otherwise = (p == q) && subsetEq s0 t0 && subsetEq s1 t1
subsetEq (B p m s0 s1) _ = False
subsetEq (L k x) t = member t k
subsetEq E t = True

mapWithKey :: (Int -> a -> b) -> IntMap a -> IntMap b
mapWithKey f E = E
mapWithKey f (L k x) = L k (f k x)
mapWithKey f (B p m t0 t1) = B p m (mapWithKey f t0) (mapWithKey f t1)

foldWithKey :: (Int -> a -> b -> b) -> b -> IntMap a -> b
foldWithKey f c E = c
foldWithKey f c (L k x) = f k x c
foldWithKey f c (B p m t0 t1) = foldWithKey f (foldWithKey f c t1) t0

filterWithKey :: (Int -> a -> Bool) -> IntMap a -> IntMap a
filterWithKey g E = E
filterWithKey g t@(L k x) = if g k x then t else E
filterWithKey g (B p m t0 t1) = 
  makeB p m (filterWithKey g t0) (filterWithKey g t1)

partitionWithKey :: (Int -> a -> Bool) -> IntMap a -> (IntMap a, IntMap a)
partitionWithKey g E = (E, E)
partitionWithKey g t@(L k x) = if g k x then (t, E) else (E, t)
partitionWithKey g (B p m t0 t1) =
  let (t0',t0'') = partitionWithKey g t0
      (t1',t1'') = partitionWithKey g t1
  in (makeB p m t0' t1', makeB p m t0'' t1'')

unionWithKey :: (Int -> a -> a -> a) -> IntMap a -> IntMap a -> IntMap a
unionWithKey f s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionWithKey f s0 t) s1
                                 else B p m s0 (unionWithKey f s1 t)
                else join p s q t
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionWithKey f s t0) t1
                                 else B q n t0 (unionWithKey f s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionWithKey f s0 t0) (unionWithKey f s1 t1)
                else join p s q t
unionWithKey f s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith (flip (f k)) k x s0) s1
                     else B p m s0 (insertWith (flip (f k)) k x s1)
    else join k (L k x) p s
unionWithKey f s@(B _ _ _ _) E = s
unionWithKey f (L k x) t = insertWith (f k) k x t
unionWithKey f E t = t

intersectWithKey :: (Int -> a -> b -> c) -> IntMap a -> IntMap b -> IntMap c
intersectWithKey f s@(B p m s0 s1) t@(B q n t0 t1)
  | m < n    = if matchPrefix q p m then
                  if zeroBit q m then intersectWithKey f s0 t
                                 else intersectWithKey f s1 t
                else E
  | m > n    = if matchPrefix p q n then
                  if zeroBit p n then intersectWithKey f s t0
                                 else intersectWithKey f s t1
                else E
  | otherwise = if p /= q then E
                else makeB p m (intersectWithKey f s0 t0) (intersectWithKey f s1 t1)
intersectWithKey f (B p m s0 s1) (L k y) =
    case lookupM (if zeroBit k m then s0 else s1) k of
      Just x -> L k (f k x y)
      Nothing -> E
intersectWithKey f s@(B _ _ _ _) E = E
intersectWithKey f (L k x) t =
    case lookupM t k of
      Just y -> L k (f k x y)
      Nothing -> E
intersectWithKey f E t = E

-- defaults

insertSeq :: S.Sequence seq => seq (Int,a) -> IntMap a -> IntMap a
insertSeq = insertSeqUsingFoldr

unionSeq :: S.Sequence seq => seq (IntMap a) -> IntMap a
unionSeq = unionSeqUsingReduce

deleteAll :: Int -> IntMap a -> IntMap a
deleteAll = delete

deleteSeq :: S.Sequence seq => seq Int -> IntMap a -> IntMap a
deleteSeq = deleteSeqUsingFoldr

count :: IntMap a -> Int -> Int
count = countUsingMember

lookupAll :: S.Sequence seq => IntMap a -> Int -> seq a
lookupAll = lookupAllUsingLookupM

lookupWithDefault :: a -> IntMap a -> Int -> a
lookupWithDefault = lookupWithDefaultUsingLookupM

elements :: S.Sequence seq => IntMap a -> seq a
elements = elementsUsingFold

fromSeqWithKey :: 
    S.Sequence seq => (Int -> a -> a -> a) -> seq (Int,a) -> IntMap a
fromSeqWithKey = fromSeqWithKeyUsingInsertSeqWithKey

insertWithKey :: (Int -> a -> a -> a) -> Int -> a -> IntMap a -> IntMap a
insertWithKey = insertWithKeyUsingInsertWith

insertSeqWith :: 
    S.Sequence seq => (a -> a -> a) -> seq (Int,a) -> IntMap a -> IntMap a
insertSeqWith = insertSeqWithUsingInsertWith

insertSeqWithKey :: 
    S.Sequence seq => 
      (Int -> a -> a -> a) -> seq (Int,a) -> IntMap a -> IntMap a
insertSeqWithKey = insertSeqWithKeyUsingInsertWithKey

adjustAll :: (a -> a) -> Int -> IntMap a -> IntMap a
adjustAll = adjust

unionSeqWith :: S.Sequence seq => (a -> a -> a) -> seq (IntMap a) -> IntMap a
unionSeqWith = unionSeqWithUsingReduce

toSeq :: S.Sequence seq => IntMap a -> seq (Int,a)
toSeq = toSeqUsingFoldWithKey

keys :: S.Sequence seq => IntMap a -> seq Int
keys = keysUsingFoldWithKey
  
unionSeqWithKey :: 
    S.Sequence seq => (Int -> a -> a -> a) -> seq (IntMap a) -> IntMap a
unionSeqWithKey = unionSeqWithKeyUsingReduce

-- instance declarations

instance A.AssocX FM Int where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert; 
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq; 
   null = null; size = size; member = member; count = count; 
   lookup = lookup; lookupM = lookupM; lookupAll = lookupAll; 
   lookupWithDefault = lookupWithDefault; adjust = adjust; 
   adjustAll = adjustAll; map = map; fold = fold; fold1 = fold1; 
   filter = filter; partition = partition; elements = elements;
   instanceName m = moduleName}

instance A.Assoc FM Int where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey; 
   foldWithKey = foldWithKey; filterWithKey = filterWithKey; 
   partitionWithKey = partitionWithKey}

instance A.FiniteMapX FM Int where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey; 
   insertWith = insertWith; insertWithKey = insertWithKey; 
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey; 
   unionl = unionl; unionr = unionr; unionWith = unionWith; 
   unionSeqWith = unionSeqWith; intersectWith = intersectWith; 
   difference = difference; subset = subset; subsetEq = subsetEq}

instance A.FiniteMap FM Int where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey; 
   intersectWithKey = intersectWithKey}

instance Functor (FM Int) where
  fmap = map

