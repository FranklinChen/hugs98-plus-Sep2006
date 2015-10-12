------------------------------------------------------------------------------
-- Demonstration of the graph algorithms described in:
-- 
-- ``Lazy Depth-First Search and Linear Graph Algorithms in Haskell''
--   by David King and John Launchbury
-- 
-- Most of the code is in the library modules Data.Graph and Data.Tree.
--
-- Suitable for use with Hugs 98.
------------------------------------------------------------------------------

module Ldfs
	( figure4, {- figure5, -} figure7
	) where

import Data.Char
import Data.Graph
import Data.Tree

graph = buildG (ord 'a',ord 'j') (reverse [(ord v, ord w) | (v,w) <- vs])
	where vs = [ ('a', 'b'),  ('a', 'f'),  ('b', 'c'),
		     ('b', 'e'),  ('c', 'a'),  ('c', 'd'),
		     ('e', 'd'),  ('g', 'h'),  ('g', 'j'),
		     ('h', 'f'),  ('h', 'i'),  ('h', 'j') ]

figure4 = buildG (ord 'a',ord 'i') ([(ord v, ord w) | (v,w) <- vs] ++ reverse [ (ord v, ord w) | (w, v) <- vs ])
          where vs = [ ('b', 'a'), ('e', 'a'), ('c', 'b'),
                       ('d', 'c'), ('b', 'd'), ('f', 'e'),
                       ('h', 'e'), ('g', 'f'), ('e', 'g'),
                       ('i', 'h'), ('a', 'i'), ('h', 'a') ]

{-
figure5 = map (do_label figure4 dnum) f
          where f    = dff figure4
                dnum = preArr (bounds figure4) f
-}

figure7 = map (fmap (map chr)) $ bcc figure4
