-- Substitutions and Unification of Prolog Terms
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module Subst where

import Prolog

infixr 3 @@
infix  4 ->-

--- Substitutions:

type Subst = Id -> Term

-- substitutions are represented by functions mapping identifiers to terms.
--
-- app s     extends the substitution s to a function mapping terms to terms
-- nullSubst is the empty substitution which maps every identifier to the
--           same identifier (as a term).
-- i ->- t   is the substitution which maps the identifier i to the term t,
--           but otherwise behaves like nullSubst.
-- s1@@ s2  is the composition of substitutions s1 and s2
--           N.B.  app is a monoid homomorphism from (Subst,nullSubst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  app (s1 @@ s2) = app s1 . app s2
--                 s @@ nullSubst = s = nullSubst @@ s

app                     :: Subst -> Term -> Term
app s (Var i)            = s i
app s (Struct a ts)      = Struct a (map (app s) ts)

nullSubst               :: Subst
nullSubst i              = Var i

(->-)                   :: Id -> Term -> Subst
(i ->- t) j | j==i       = t
            | otherwise  = Var j

(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = app s1 . s2 

--- Unification:

-- unify t1 t2 returns a list containing a single substitution s which is
--             the most general unifier of terms t1 t2.  If no unifier
--             exists, the list returned is empty.

unify :: Term -> Term -> [Subst]
unify (Var x)       (Var y)       = if x==y then [nullSubst] else [x->-Var y]
unify (Var x)       t2            = [ x ->- t2 | x `notElem` varsIn t2 ]
unify t1            (Var y)       = [ y ->- t1 | y `notElem` varsIn t1 ]
unify (Struct a ts) (Struct b ss) = [ u | a==b, u<-listUnify ts ss ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify []     []     = [nullSubst]
listUnify []     (r:rs) = []
listUnify (t:ts) []     = []
listUnify (t:ts) (r:rs) = [ u2 @@ u1 | u1<-unify t r,
                                       u2<-listUnify (map (app u1) ts)
                                                     (map (app u1) rs) ]

--- End of Subst.hs
