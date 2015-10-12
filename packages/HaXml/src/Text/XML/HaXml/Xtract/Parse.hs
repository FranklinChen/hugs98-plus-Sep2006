-- | A parser for the Xtract command-language.  (The string input is
--   tokenised internally by the lexer 'lexXtract'.)
--   See <http://www.haskell.org/HaXml/Xtract.html> for the grammar that
--   is accepted.

--   Because the original Xtract grammar was left-recursive, we have
--   transformed it into a non-left-recursive form.
module Text.XML.HaXml.Xtract.Parse (parseXtract,xtract) where

import Text.ParserCombinators.Poly hiding (bracket)
import Text.XML.HaXml.Xtract.Lex
import Text.XML.HaXml.Xtract.Combinators
import Text.XML.HaXml.Combinators
import List(isPrefixOf)

-- | To convert an Xtract query into an ordinary HaXml combinator expression.
xtract :: String -> CFilter i
xtract query = dfilter (parseXtract query)

-- | The cool thing is that the Xtract command parser directly builds
--   a higher-order 'DFilter' (see "Text.XML.HaXml.Xtract.Combinators")
--   which can be applied to an XML document without further ado.
--   (@parseXtract@ halts the program if a parse error is found.)
parseXtract :: String -> DFilter i
parseXtract = either error id . parseXtract'

-- | @parseXtract'@ returns error messages through the Either type.
parseXtract' :: String -> Either String (DFilter i)
parseXtract' = fst . runParser xql . lexXtract

xql = aquery (local keep)

---- Auxiliary Parsing Functions ----
type XParser a = Parser (Either String (Posn,TokenT)) a

string :: XParser String
string = P (\inp -> case inp of
                (Left err: _) -> (Left (False,err), inp)
                (Right (p,TokString n):ts) -> (Right n, ts)
                ts -> (Left (False,"expected a string"), ts) )
number :: XParser Integer
number = P (\inp -> case inp of
                (Left err: _) -> (Left (False,err), inp)
                (Right (p,TokNum n):ts) -> (Right n, ts)
                ts -> (Left (False,"expected a number"), ts) )
symbol :: String -> XParser ()
symbol s = P (\inp -> case inp of
                (Left err: _) -> (Left (False,err), inp)
                (Right (p, Symbol n):ts) | n==s -> (Right (), ts)
                ts -> (Left (False,"expected symbol "++s), ts) )

quote = oneOf [ symbol "'",  symbol "\"" ]

pam fs x = [ f x | f <- fs ]


{--- original Xtract grammar ----
      query     = string			tagname
                | string *			tagname prefix
                | * string			tagname suffix
                | *				any element
                | -				chardata
                | ( query )
                | query / query			parent/child relationship
                | query // query		deep inside
                | query + query			union of queries
                | query [predicate]
                | query [positions]

      predicate = quattr			has attribute
                | quattr op ' string '		attribute has value
                | quattr op " string "		attribute has value
                | quattr op  quattr		attribute value comparison (lexical)
                | quattr nop integer  		attribute has value (numerical)
                | quattr nop quattr		attribute value comparison (numerical)
                | ( predicate )			bracketting
                | predicate & predicate		logical and
                | predicate | predicate		logical or
                | ~ predicate			logical not

      attribute = @ string			has attribute
                | query / @ string		child has attribute
                | -				has textual content
                | query / -			child has textual content

      quattr    = query
                | attribute

      op        =  =				equal to
                |  !=				not equal to
                |  <				less than
                |  <=				less than or equal to
                |  >				greater than
                |  >=				greater than or equal to

      nop       =  .=.				equal to
                |  .!=.				not equal to
                |  .<.				less than
                |  .<=.				less than or equal to
                |  .>.				greater than
                |  .>=.				greater than or equal to

      positions = position {, positions}	multiple positions
                | position - position		ranges

      position  = integer			numbering is from 0 upwards
                | $				last


---- transformed grammar (removing left recursion)
      aquery = ./ tquery	-- current context
             | tquery		-- also current context
             | / tquery		-- root context
             | // tquery	-- deep context from root

      tquery = ( tquery ) xquery
             | tag xquery
             | -		-- fixes original grammar ("-/*" is incorrect)
      
      tag    = string *
             | string
             | * string
             | *
      
      xquery = / tquery
             | // tquery
             | / @ string	-- new: print attribute value
             | + tquery
             | [ tpredicate ] xquery
             | [ positions ] xquery
             | lambda

      tpredicate = vpredicate upredicate
      upredicate = & tpredicate
                 | | tpredicate
                 | lambda
      vpredicate = ( tpredicate )
                 | ~ tpredicate
                 | tattribute

      tattribute = aquery uattribute
                 | @ string vattribute
      uattribute = / @ string vattribute
                 | vattribute
      vattribute = op wattribute
                 | op ' string '
                 | nop wattribute
                 | nop integer
                 | lambda
      wattribute = @ string
                 | aquery / @ string
                 | aquery

      positions  = simplepos commapos
      simplepos  = integer range
                 | $
      range      = - integer
                 | - $
                 | lambda
      commapos   = , simplepos commapos
                 | lambda

      op         =  =
                 |  !=
                 |  <
                 |  <=
                 |  >
                 |  >=

      nop        =  .=.
                 |  .!=.
                 |  .<.
                 |  .<=.
                 |  .>.
                 |  .>=.
-}

bracket :: XParser a -> XParser a
bracket p =
  do symbol "("
     x <- p
     symbol ")"
     return x


---- Xtract parsers ----

-- aquery takes a localisation filter, but if the query string starts
-- from the root, we ignore the local context
aquery :: DFilter i -> XParser (DFilter i)
aquery localise = oneOf
    [ do symbol "//"
         tquery [oglobo deep]
    , do symbol "/"
         tquery [oglobo id]
    , do symbol "./"
         tquery [(localise //>>)]
    , do tquery [(localise //>>)]
    ]

tquery :: [DFilter i->DFilter i] -> XParser (DFilter i)
tquery [] = tquery [id]
tquery (qf:cxt) = oneOf
    [ do q <- bracket (tquery (qf:qf:cxt))
         xquery cxt q
    , do q <- xtag
         xquery cxt (qf q)
    , do symbol "-"
         return (qf (local txt))
    ]

xtag :: XParser (DFilter i)
xtag = oneOf
    [ do s <- string
         symbol "*"
         return (local (tagWith (s `isPrefixOf`)))
    , do s <- string
         return (local (tag s))
    , do symbol "*"
         s <- string
         return (local (tagWith (((reverse s) `isPrefixOf`) . reverse)))
    , do symbol "*"
         return (local elm)
    ]


xquery :: [DFilter i->DFilter i] -> DFilter i -> XParser (DFilter i)
xquery cxt q1 = oneOf
    [ do symbol "/"
         ( do symbol "@"
              attr <- string
              return (oiffindo attr (\s->local (literal s)) ononeo `ooo` q1)
           `onFail`
           tquery ((q1 //>>):cxt) )
    , do symbol "//"
         tquery ((\q2-> (oloco deep) q2 `ooo` local children `ooo` q1):cxt)
    , do symbol "+"
         q2 <- tquery cxt
         return (ocato [q1,q2])
    , do symbol "["
         is <- iindex	-- now extended to multiple indexes
         symbol "]"
         xquery cxt (\xml-> concat . pam is . q1 xml)
    , do symbol "["
         p <- tpredicate
         symbol "]"
         xquery cxt (q1 `owitho` p)
    , return q1
    ]

tpredicate :: XParser (DFilter i)
tpredicate =
  do p <- vpredicate
     f <- upredicate
     return (f p)

upredicate :: XParser (DFilter i->DFilter i)
upredicate = oneOf
    [ do symbol "&"
         p2 <- tpredicate
         return (`ooo` p2)
    , do symbol "|"
         p2 <- tpredicate
         return (||>|| p2)
    , return id
    ]

vpredicate :: XParser (DFilter i)
vpredicate = oneOf
    [ do bracket tpredicate
    , do symbol "~"
         p <- tpredicate
         return (local keep `owithouto` p)
    , do tattribute
    ]

tattribute :: XParser (DFilter i)
tattribute = oneOf
    [ do q <- aquery (local keep)
         uattribute q
    , do symbol "@"
         s <- string
         vattribute (local keep, local (attr s), oiffindo s)
    ]

uattribute :: DFilter i -> XParser (DFilter i)
uattribute q = oneOf
    [ do symbol "/"
         symbol "@"
         s <- string
         vattribute (q, local (attr s), oiffindo s)
    , do vattribute (q, local keep,     oifTxto)
    ]

vattribute :: (DFilter i, DFilter i, (String->DFilter i)->DFilter i->DFilter i)
              -> XParser (DFilter i)
vattribute (q,a,iffn) = oneOf
  [ do cmp <- op
       quote
       s2 <- string
       quote
       return ((iffn (\s1->if cmp s1 s2 then okeepo else ononeo) ononeo)
               `ooo` q)
  , do cmp <- op
       (q2,iffn2) <- wattribute
       return ((iffn (\s1-> iffn2 (\s2-> if cmp s1 s2 then okeepo else ononeo)
                                  ononeo)
                     ononeo) `ooo` q)
  , do cmp <- nop
       n <- number
       return ((iffn (\s->if cmp (read s) n then okeepo else ononeo) ononeo)
               `ooo` q)
  , do cmp <- nop
       (q2,iffn2) <- wattribute
       return ((iffn (\s1-> iffn2 (\s2-> if cmp (read s1) (read s2) then okeepo
                                                                    else ononeo)
                                  ononeo)
                     ononeo) `ooo` q)
  , do return ((a `ooo` q))
  ]

wattribute :: XParser (DFilter i, (String->DFilter i)->DFilter i->DFilter i)
wattribute = oneOf
    [ do symbol "@"
         s <- string
         return (okeepo, oiffindo s)
    , do q <- aquery okeepo
         symbol "/"
         symbol "@"
         s <- string
         return (q, oiffindo s)
    , do q <- aquery okeepo
         return (q, oifTxto)
    ]


iindex :: XParser [[a]->[a]]
iindex =
    do i <- simpleindex
       is <- idxcomma
       return (i:is)

simpleindex :: XParser ([a]->[a])
simpleindex = oneOf
    [ do n <- number
         r <- rrange n
         return r
    , do symbol "$"
         return (keep . last)
    ]

rrange, numberdollar :: Integer -> XParser ([a]->[a])
rrange n1 = oneOf
    [ do symbol "-"
         numberdollar n1
    , return (keep.(!!(fromInteger n1)))
    ]

numberdollar n1 = oneOf
    [ do n2 <- number
         return (take (fromInteger (1+n2-n1)) . drop (fromInteger n1))
    , do symbol "$"
         return (drop (fromInteger n1))
    ]

idxcomma :: XParser [[a]->[a]]
idxcomma = oneOf
    [ do symbol ","
         r <- simpleindex
         rs <- idxcomma
         return (r:rs)
    , return []
    ]


op :: XParser (String->String->Bool)
op = oneOf
    [ do symbol "=";  return (==)
    , do symbol "!="; return (/=)
    , do symbol "<";  return (<)
    , do symbol "<="; return (<=)
    , do symbol ">";  return (>)
    , do symbol ">="; return (>=)
    ]

nop :: XParser (Integer->Integer->Bool)
nop = oneOf
    [ do symbol ".=.";  return (==)
    , do symbol ".!=."; return (/=)
    , do symbol ".<.";  return (<)
    , do symbol ".<=."; return (<=)
    , do symbol ".>.";  return (>)
    , do symbol ".>=."; return (>=)
    ]

