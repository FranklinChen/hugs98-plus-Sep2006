{-
By Donald A. Smith, December 22, 1994, based on Mark Jones' PureEngine.

This inference engine implements a variation of the Andorra Principle for
logic programming. (See references at the end of this file.) The basic
idea is that instead of always selecting the first goal in the current
list of goals, select a relatively deterministic goal.

For each goal g in the list of goals, calculate the resolvents that would
result from selecting g.  Then choose a g which results in the lowest
number of resolvents.  If some g results in 0 resolvents then fail.
(This would occur for a goal like:  ?- append(A,B,[1,2,3]),equals(1,2).)
Prolog would not perform this optimization and would instead search
and backtrack wastefully.  If some g results in a single resolvent
(i.e., only a single clause matches) then that g will get selected;
by selecting and resolving g, bindings are propagated sooner, and useless
search can be avoided, since these bindings may prune away choices for
other clauses.  For example: ?- append(A,B,[1,2,3]),B=[].
-}

module AndorraEngine( version, prove ) where

import Prolog
import Subst

version = "Andorra Principle Interpreter (select deterministic goals first)"

solve   :: Database -> Int -> Subst -> [Term] -> [Subst]
solve db = slv where
   slv           :: Int -> Subst -> [Term] -> [Subst]
   slv n s [] = [s]
   slv n s goals =
    let allResolvents = resolve_selecting_each_goal goals db n in
      let (gs,gres) =  findMostDeterministic allResolvents in
          concat [slv (n+1) (u@@s) (map (app u) (tp++gs)) | (u,tp) <- gres]

resolve_selecting_each_goal::
    [Term] -> Database -> Int -> [([Term],[(Subst,[Term])])]
--  For each pair in the list that we return, the first element of the
--  pair is the list of unresolved goals; the second element is the list
--  of resolvents of the selected goal, where a resolvent is a pair
--  consisting of a substitution and a list of new goals.
resolve_selecting_each_goal goals db n = [(gs, gResolvents) |
      (g,gs) <- delete goals, let gResolvents = resolve db g n]

-- The unselected goals from above are not passed in.
resolve :: Database -> Term -> Int -> [(Subst,[Term])]
resolve db g n = [(u,tp) | (tm:-tp)<-renClauses db n g, u<-unify g tm]
-- u is not yet applied to tp, since it is possible that g won't be selected.
-- Note that unify could be nondeterministic.

findMostDeterministic:: [([Term],[(Subst,[Term])])] -> ([Term],[(Subst,[Term])])
findMostDeterministic  allResolvents = minF comp allResolvents where
   comp:: (a,[b]) -> (a,[b]) -> Bool
   comp (_,gs1) (_,gs2) = (length gs1) < (length gs2)
-- It seems to me that there is an opportunity for a clever compiler to
-- optimize this code a lot. In particular, there should be no need to
-- determine the total length of a goal list if it is known that
-- there is a shorter goal list in allResolvents ... ?

delete ::  [a] -> [(a,[a])]
delete l = d l [] where
   d :: [a] -> [a] ->  [(a,[a])]
   d [g] sofar = [ (g,sofar) ]
   d (g:gs) sofar = (g,sofar++gs) : (d gs (g:sofar))

minF               :: (a -> a -> Bool) -> [a] -> a
minF f (h:t) = m h t where
--   m :: a -> [a] -> a
     m sofar [] = sofar
     m sofar (h:t) = if (f h sofar) then m h t else m sofar t

prove    :: Database -> [Term] -> [Subst]
prove db  = solve db 1 nullSubst

{- An optimized, incremental version of the above interpreter would use
  a data representation in which for each goal in "goals" we carry around
  the list of resolvents.  After each resolution step we update the lists.
-}

{- References

   Seif Haridi & Per Brand, "Andorra Prolog, an integration of Prolog
   and committed choice languages" in Proceedings of FGCS 1988, ICOT,
   Tokyo, 1988.

   Vitor Santos Costa, David H. D. Warren, and Rong Yang, "Two papers on
   the Andorra-I engine and preprocessor", in Proceedings of the 8th
   ICLP. MIT Press, 1991.

   Steve Gregory and Rong Yang, "Parallel Constraint Solving in
   Andorra-I", in Proceedings of FGCS'92. ICOT, Tokyo, 1992.

   Sverker Janson and Seif Haridi, "Programming Paradigms of the Andorra
   Kernel Language", in Proceedings of ILPS'91. MIT Press, 1991.

   Torkel Franzen, Seif Haridi, and Sverker Janson, "An Overview of the
   Andorra Kernel Language", In LNAI (LNCS) 596, Springer-Verlag, 1992.
-}
