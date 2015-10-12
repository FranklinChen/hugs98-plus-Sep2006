-- This program can be used to solve exercise 1.2.1 in Bird & Wadler's
-- ``Introduction to functional programming'' ....
--
-- Write down the ways to reduce sqr (sqr (3+7)) to normal form
-- (without assuming shared evaluation of function arguments).

module EvalRed where

data  Term  = Square Term      -- The square of a term
            | Plus Term Term   -- The sum of two terms
            | Times Term Term  -- The product of two terms
            | Num Int          -- A numeric constant

instance Show Term where
    showsPrec p (Square t)  = showString "sqr " . shows t
    showsPrec p (Plus n m)  = showChar '(' . shows n . showChar '+'
                                           . shows m . showChar ')'
    showsPrec p (Times n m) = showChar '(' . shows m . showChar '*'
                                           . shows n . showChar ')'
    showsPrec p (Num i)     = shows i


-- What are the subterms of a given term?

type Subterm                     = (Term,           -- The subterm expression
                                    Term->Term)     -- A function which embeds
                                                    -- it back in the original
                                                    -- term

rebuild                         :: Subterm -> Term
rebuild (t, embed)               = embed t

subterms                        :: Term -> [Subterm]
subterms t                       = [ (t,id) ] ++ properSubterms t

properSubterms                  :: Term -> [Subterm]
properSubterms (Square t)        = down Square (subterms t)
properSubterms (Plus t1 t2)      = down (flip Plus t2)  (subterms t1) ++
                                   down (Plus t1)       (subterms t2)
properSubterms (Times t1 t2)     = down (flip Times t2) (subterms t1) ++
                                   down (Times t1)      (subterms t2)
properSubterms (Num n)           = []

down                            :: (Term -> Term) -> [Subterm] -> [Subterm]
down f                           = map (\(t, e) -> (t, f.e))


-- Some (semi-)general variations on standard themes:

filter'                         :: (a -> Bool) -> [(a, b)] -> [(a, b)]
filter' p                        = filter (p.fst)

map'                            :: (a -> b) -> [(a, c)] -> [(b, c)]
map' f                           = map (\(a, c) -> (f a, c))


-- Reductions:

isRedex                         :: Term -> Bool
isRedex (Square _)               = True
isRedex (Plus (Num _) (Num _))   = True
isRedex (Times (Num _) (Num _))  = True
isRedex _                        = False

contract                        :: Term -> Term
contract (Square t)              = Times t t
contract (Plus (Num n) (Num m))  = Num (n+m)
contract (Times (Num n) (Num m)) = Num (n*m)
contract _                       = error "Not a redex!"

singleStep        :: Term -> [Term]
singleStep         = map rebuild . map' contract . filter' isRedex . subterms

normalForms       :: Term -> [Term]
normalForms t 
       | null ts   = [ t ]
       | otherwise = [ n | t'<-ts, n<-normalForms t' ]
                     where ts = singleStep t

redSequences      :: Term -> [[Term]]
redSequences t
       | null ts   = [ [t] ]
       | otherwise = [ t:rs | t'<-ts, rs<-redSequences t' ]
                     where ts = singleStep t


-- Particular example:

term0 = Square (Square (Plus (Num 3) (Num 7)))
nfs0  = normalForms term0
rsq0  = redSequences term0

-- Using Hugs:
--
-- ? length nfs0
-- 547
-- ?
--
