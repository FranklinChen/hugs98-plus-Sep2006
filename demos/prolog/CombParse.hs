-----------------------------------------------------------------------------
-- Combinator parsing library:
--
-- The original Gofer version of this file was based on Richard Bird's
-- parselib.orw for Orwell (with a number of extensions).
--
-- Not recommended for new work.
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module CombParse where

infixr 6 `pseq`
infixl 5 `pam`
infixr 4 `orelse`

--- Type definition:

type Parser a = [Char] -> [(a,[Char])]

-- A parser is a function which maps an input stream of characters into
-- a list of pairs each containing a parsed value and the remainder of the
-- unused input stream.  This approach allows us to use the list of
-- successes technique to detect errors (i.e. empty list ==> syntax error).
-- it also permits the use of ambiguous grammars in which there may be more
-- than one valid parse of an input string.

--- Primitive parsers:

-- pfail    is a parser which always fails.
-- okay v   is a parser which always succeeds without consuming any characters
--          from the input string, with parsed value v.
-- tok w    is a parser which succeeds if the input stream begins with the
--          string (token) w, returning the matching string and the following
--          input.  If the input does not begin with w then the parser fails.
-- sat p    is a parser which succeeds with value c if c is the first input
--          character and c satisfies the predicate p.

pfail       :: Parser a 
pfail is     = []

okay        :: a -> Parser a  
okay v is    = [(v,is)]

tok         :: [Char] -> Parser [Char]
tok w is     = [(w, drop n is) | w == take n is]
               where n = length w

sat         :: (Char -> Bool) -> Parser Char 
sat p []     = []
sat p (c:is) = [ (c,is) | p c ]

--- Parser combinators:

-- p1 `orelse` p2 is a parser which returns all possible parses of the input
--                string, first using the parser p1, then using parser p2.
-- p1 `seq` p2    is a parser which returns pairs of values (v1,v2) where
--                v1 is the result of parsing the input string using p1 and
--                v2 is the result of parsing the remaining input using p2.
-- p `pam` f      is a parser which behaves like the parser p, but returns
--                the value f v wherever p would have returned the value v.
--
-- just p         is a parser which behaves like the parser p, but rejects any
--                parses in which the remaining input string is not blank.
-- sp p           behaves like the parser p, but ignores leading spaces.
-- sptok w        behaves like the parser tok w, but ignores leading spaces.
--
-- many p         returns a list of values, each parsed using the parser p.
-- many1 p        parses a non-empty list of values, each parsed using p.
-- listOf p s     parses a list of input values using the parser p, with
--                separators parsed using the parser s.

orelse             :: Parser a -> Parser a -> Parser a 
(p1 `orelse` p2) is = p1 is ++ p2 is
 
pseq               :: Parser a -> Parser b -> Parser (a,b)
(p1 `pseq` p2) is   = [((v1,v2),is2) | (v1,is1) <- p1 is, (v2,is2) <- p2 is1]

pam                :: Parser a -> (a -> b) -> Parser b 
(p `pam` f) is      = [(f v, is1) | (v,is1) <- p is]

just               :: Parser a -> Parser a
just p is           = [ (v,"") | (v,is')<- p is, dropWhile (' '==) is' == "" ]

sp                 :: Parser a -> Parser a
sp p                = p . dropWhile (' '==)

sptok              :: [Char] -> Parser [Char]
sptok               =  sp . tok

many               :: Parser a  -> Parser [a]
many p              = q
                      where q = ((p `pseq` q) `pam` makeList) `orelse` (okay [])

many1              :: Parser a -> Parser [a]
many1 p             = p `pseq` many p `pam` makeList

listOf             :: Parser a -> Parser b -> Parser [a]
listOf p s          = p `pseq` many (s `pseq` p) `pam` nonempty
                      `orelse` okay []
                      where nonempty (x,xs) = x:(map snd xs)

--- Internals:

makeList       :: (a,[a]) -> [a]
makeList (x,xs) = x:xs

-----------------------------------------------------------------------------
