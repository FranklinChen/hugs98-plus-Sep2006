-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module EdisonPrelude
	{-# DEPRECATED "This module is unmaintained, and will disappear soon" #-}
	where

data Maybe2 a b = Just2 a b | Nothing2      deriving (Eq,Show)
data Maybe3 a b c = Just3 a b c | Nothing3  deriving (Eq,Show)

-- utilities on Maybe2 and Maybe3 types???

class Eq a => Hash a where
  hash :: a -> Int
  -- forall x,y :: a. (x == y) implies (hash x == hash y)

class Hash a => UniqueHash a
  -- no new methods, just a stronger invariant
  -- forall x,y :: a. (x == y) iff (hash x == hash y)

class UniqueHash a => ReversibleHash a where
  unhash :: Int -> a
  -- forall x :: a. unhash (hash x) == x

  -- Note that 
  --   hash (unhash i) == i
  -- does not necessarily hold because unhash is not necessarily
  -- defined for all i, only for all i in the range of hash.


-- add a few instance declarations for ints, floats, bools, chars, etc.
