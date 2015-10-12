-----------------------------------------------------------------------------
-- Standard Library: Array operations
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Hugs.Array ( 
    module Data.Ix,  -- export all of Ix 
    unsafeIndex,

    Array, array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, ixmap,
    unsafeArray, unsafeAt, unsafeReplace, unsafeAccum, unsafeAccumArray
    ) where

import Data.Ix
import Hugs.Prelude( unsafeIndex )

infixl 9  !, //

data Array a b -- Arrays are implemented as a primitive type

array          :: Ix a => (a,a) -> [(a,b)] -> Array a b
listArray      :: Ix a => (a,a) -> [b] -> Array a b
(!)	       :: Ix a => Array a b -> a -> b
bounds         :: Ix a => Array a b -> (a,a)
indices        :: Ix a => Array a b -> [a]
elems          :: Ix a => Array a b -> [b]
assocs	       :: Ix a => Array a b -> [(a,b)]
(//)           :: Ix a => Array a b -> [(a,b)] -> Array a b
accum          :: Ix a => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
accumArray     :: Ix a => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
ixmap	       :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

primitive primArray :: (a,a) -> Int -> [(Int,b)] -> Array a b
primitive primUpdate :: [(Int,b)] -> Array a b -> Array a b
primitive primAccum :: [(Int,c)] -> Array a b -> (b -> c -> b) -> Array a b
primitive primAccumArray
    :: (a,a) -> Int -> (b -> c -> b) -> b -> [(Int,c)] -> Array a b
primitive primSubscript :: Array a b -> Int -> b

primitive primBounds :: Array a b -> (a,a)
primitive primElems  :: Array a b -> [b]
primitive primAmap   :: (b -> c) -> Array a b -> Array a c

unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray bnds	= primArray bnds (rangeSize bnds)

unsafeAt :: Ix i => Array i e -> Int -> e
unsafeAt		= primSubscript

unsafeReplace :: Ix i => Array i e -> [(Int, e)] -> Array i e
unsafeReplace iarr ies	= primUpdate ies iarr

unsafeAccum :: Ix i => (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f iarr ies	= primAccum ies iarr f

unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(Int, a)] -> Array i e
unsafeAccumArray f z bnds = primAccumArray bnds (rangeSize bnds) f z

indexAll :: Ix i => (i,i) -> [(i, a)] -> [(Int, a)]
indexAll bnds ivs = [(index bnds i,v) | (i,v) <- ivs]

array bnds          = unsafeArray bnds . indexAll bnds
listArray bnds vs   = unsafeArray bnds (zip [0..rangeSize bnds-1] vs)
arr!i               = unsafeAt arr (index (bounds arr) i)
bounds              = primBounds
indices	            = range . bounds
elems               = primElems
assocs a            = zip (indices a) (elems a)
accumArray f z bnds = unsafeAccumArray f z bnds . indexAll bnds
a // ivs            = unsafeReplace a (indexAll (bounds a) ivs)
accum f a ivs       = unsafeAccum f a (indexAll (bounds a) ivs)
ixmap bnds f arr =
    unsafeArray bnds [(unsafeIndex bnds i, arr ! f i) | i <- range bnds]

instance (Ix a) => Functor (Array a) where
    fmap = primAmap

instance (Ix a, Eq b) => Eq (Array a b) where
    a == a'   =   assocs a == assocs a'

instance (Ix a, Ord b) => Ord (Array a b) where
    a <= a'   =   assocs a <= assocs a'

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	     (\r -> [(array b as, u) | ("array",s) <- lex r,
				       (b,t)       <- reads s,
				       (as,u)      <- reads t   ])

-----------------------------------------------------------------------------
