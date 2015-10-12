-- | This is a new set of XML combinators for Xtract, not standard,
--   but based on the standard set in "Text.Xml.Haxml.Combinators".
--   The main difference is that the Content Filter type becomes a
--   Double Filter.  A Double Filter always takes the whole document
--   as an extra argument, so you can start to traverse it again from
--   any inner location within the document tree.
--
--   The new combinators definitions are derived from the old ones.
--   New names are derived from the old by surrounding with the letter @o@,
--   or by doubling the operator symbols.

module Text.XML.HaXml.Xtract.Combinators where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators


-- | double content filter - takes document root + local subtree.
type DFilter i = Content i -> Content i -> [Content i]

-- | lift an ordinary content filter to a double filter.
local,global :: CFilter i -> DFilter i
local  f = \xml sub-> f sub
global f = \xml sub-> f xml

-- | drop a double filter to an ordinary content filter
dfilter :: DFilter i -> CFilter i
dfilter f = \xml-> f xml xml

-- | lift a CFilter combinator to a DFilter combinator
oloco, oglobo :: (CFilter i->CFilter i) -> (DFilter i->DFilter i)
oloco ff  = \df-> \xml sub-> (ff (df xml)) sub
oglobo ff = \df-> \xml sub-> (ff (df xml)) xml

-- | lifted composition over double filters.
ooo :: DFilter i -> DFilter i -> DFilter i
g `ooo` f = \xml-> concatMap (g xml) . (f xml)

-- | lifted choice.
(||>||) :: (a->b->[c]) -> (a->b->[c]) -> (a->b->[c])
f ||>|| g = \xml sub-> let first = f xml sub in
                       if null first then g xml sub else first

-- | lifted predicates.
owitho, owithouto :: DFilter i -> DFilter i -> DFilter i
f `owitho` g    = \xml-> filter (not.null.g xml) . f xml
f `owithouto` g = \xml-> filter     (null.g xml) . f xml

-- | lifted unit and zero.
okeepo, ononeo :: DFilter i
okeepo = \xml sub-> [sub]	-- local keep
ononeo = \xml sub-> []		-- local none

ochildreno, oelmo, otxto :: DFilter i
ochildreno = local children
oelmo      = local elm
otxto      = local txt

applypred :: CFilter i -> DFilter i -> CFilter i
applypred f p = \xml-> (const f `owitho` p) xml xml

oiffindo :: String -> (String -> DFilter i) -> DFilter i -> DFilter i
oiffindo key yes no xml c@(CElem (Elem _ as _) _) =
  case (lookup key as) of
    Nothing -> no xml c
    (Just (AttValue [Left s])) -> yes s xml c
oiffindo key yes no xml other = no xml other

oifTxto :: (String->DFilter i) -> DFilter i -> DFilter i
oifTxto yes no xml c@(CString _ s _) = yes s xml c
oifTxto yes no xml c                 = no xml c

ocato :: [a->b->[c]] -> (a->b->[c])
ocato fs = \xml sub-> concat [ f xml sub | f <- fs ]

(//>>) :: DFilter i -> DFilter i -> DFilter i
f //>> g = g `ooo` ochildreno `ooo` f

(<<//) :: DFilter i -> DFilter i -> DFilter i
f <<// g = f `owitho` (g `ooo` ochildreno)

odeepo :: DFilter i -> DFilter i
odeepo f   = f ||>|| (odeepo f `ooo` ochildreno)

