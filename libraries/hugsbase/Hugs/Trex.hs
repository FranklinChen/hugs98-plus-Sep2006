-----------------------------------------------------------------------------
-- Trex utilities:  Functions to compare and show record values
--
-- Warning: This file is an integral part of the TREX implementation, and
-- should not be modified without corresponding changes in the interpreter.
--
-- Suitable for use with Hugs 98, if compiled with TREX support.
-----------------------------------------------------------------------------

module Hugs.Trex( Rec, emptyRec, EmptyRow,
	ShowRecRow(..), EqRecRow(..), insertField ) where

import Hugs.Prelude ( Rec, emptyRec, EmptyRow )

-- Code for equalities:

instance EqRecRow r => Eq (Rec r) where
  r == s = eqFields (eqRecRow r s)
           where eqFields = and . map snd

class EqRecRow r where
  eqRecRow :: Rec r -> Rec r -> [(String,Bool)]

instance EqRecRow EmptyRow where
  eqRecRow _ _ = []


-- Code for showing values:

instance ShowRecRow r => Show (Rec r) where
  showsPrec d = showFields . showRecRow
   where
    showFields   :: [(String, ShowS)] -> ShowS
    showFields [] = showString "emptyRec"
    showFields xs = showChar '(' . foldr1 comma (map fld xs) . showChar ')'
     where comma a b = a . showString ", " . b
           fld (s,v) = showString s . showString " = " . v

class ShowRecRow r where
  showRecRow :: Rec r -> [(String, ShowS)]

instance ShowRecRow EmptyRow where
  showRecRow _ = []


-- General utility:

insertField       :: String -> v -> [(String, v)] -> [(String, v)]
insertField n v fs = {- case fs of
                       []     -> [(n,v)]
                       (r:rs) -> if n <= fst r
                                   then (n,v):fs
                                   else r : insertField n v rs -}
                     bef ++ [(n,v)] ++ aft
 where (bef,aft) = span (\r -> n > fst r) fs

-----------------------------------------------------------------------------

