module Text.ParserCombinators.PolyLazy
  ( -- * A Parser datatype parameterised on arbitrary token type.
    --   Parsers do not return explicit failure.  An exception is
    --   raised instead.  This allows partial results to be returned
    --   before a full parse is complete.
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser t a -> [t] -> (a, [t])
  , failBad     -- :: String -> Parser t a
  , commit      -- :: Parser t a -> Parser t a
    -- * Combinators
    -- ** primitives
  , next	-- :: Parser t t
  , satisfy	-- :: (t->Bool) -> Parser t t
  , apply	-- :: Parser t (a->b) -> Parser t a -> Parser t b
  , discard	-- :: Parser t a -> Parser t b -> Parser t a
    -- ** error-handling
  , adjustErr	-- :: Parser t a -> (String->String) -> Parser t a
  , adjustErrBad-- :: Parser t a -> (String->String) -> Parser t a
  , indent	-- :: Int -> String -> String
    -- ** choices
  , onFail	-- :: Parser t a -> Parser t a -> Parser t a
  , oneOf	-- :: Show t => [Parser t a] -> Parser t a
  , oneOf'	-- :: [(String,Parser t a)] -> Parser t a
  , optional	-- :: Parser t a -> Parser t (Maybe a)
    -- ** sequences
  , many	-- :: Parser t a -> Parser t [a]
  , many1	-- :: Parser t a -> Parser t [a]
  , sepBy	-- :: Parser t a -> Parser t sep -> Parser t [a]
  , sepBy1	-- :: Parser t a -> Parser t sep -> Parser t [a]
  , bracketSep	-- :: Parser t bra -> Parser t sep -> Parser t ket
                --    -> Parser t a -> Parser t [a]
  , bracket	-- :: Parser t bra -> Parser t ket -> Parser t a
                --    -> Parser t a
  , manyFinally -- :: Parser t a -> Parser t z -> Parser t [a]
    -- ** re-parsing
  , reparse	-- :: [t] -> Parser t ()
  ) where

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- | The @Parser@ datatype is a fairly generic parsing monad with error
--   reporting.  It can be used for arbitrary token types, not just
--   String input.  (If you require a running state, use module PolyState
--   instead.)
newtype Parser t a = P ([t] -> (Either String a, [t]))

--   A return type like Either, that distinguishes not only between
--   right and wrong answers, but also had gradations of wrongness.
--   Not used in this library. !!!!!!!!!!!!!!!!!!!!!!!!!!!
type EitherE a b = Either (Bool,a) b

-- | Apply a parser to an input token sequence.  The parser cannot return
--   an error value explicitly, so errors raise an exception.  Thus, results
--   can be partial (lazily constructed, but containing undefined).
runParser :: Parser t a -> [t] -> (a, [t])
runParser (P p) =
    (\ (e,ts)-> (case e of {Left m->throwE m; Right x->x}, ts) )
    . p

instance Functor (Parser t) where
    fmap f (P p) = P (\ts-> case p ts of
                                (Left msg, ts') -> (Left msg,    ts')
                                (Right x,  ts') -> (Right (f x), ts'))
instance Monad (Parser t) where
    return x     = P (\ts-> (Right x, ts))
    (P f) >>= g  = P (\ts-> case f ts of
                                (Left msg, ts') -> (Left msg, ts')
                                (Right x,  ts') -> let (P g') = g x in g' ts')
    fail e       = P (\ts-> (Left e, ts))

-- | Simple failure can be corrected, but when a simple fail is not strong
--   enough, use failBad for emphasis.  It guarantees parsing will
--   terminate with an exception.

failBad :: String -> Parser t a
failBad msg      = P (\ts-> (throwE msg, ts))

-- | Commit is a way of raising the severity of any errors found within
--   its argument.  Used in the middle of a parser definition, it means that
--   any operations prior to commitment fail softly, but after commitment,
--   they fail hard.
commit :: Parser t a -> Parser t a
commit (P p) = P (\ts-> case p ts of
                            (Left e, ts') -> (throwE e, ts')
                            right         -> right )


-- Combinators

-- | One token
next :: Parser t t
next = P (\ts-> case ts of
                    []  -> (Left "Ran out of input (EOF)", [])
                    (t:ts') -> (Right t, ts') )

-- | One token satifying a predicate
satisfy :: (t->Bool) -> Parser t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }

infixl 3 `apply`
-- | Apply a parsed function to a parsed value
apply :: Parser t (a->b) -> Parser t a -> Parser t b
--pf `apply` px = do { f <- pf; x <- px; return (f x) }
-- Needs to be lazier!  Must not force the argument value too early. 
(P pf) `apply` (P px) = P (\ts->
    case pf ts of
      (Left msg, ts') -> (Left msg, ts')
      (Right f,  ts') -> let (x',ts'') = px ts'
                             x = case x' of { Right x -> x; Left e -> throwE e }
                         in (Right (f x), ts'') )

infixl 3 `discard`
-- | @x `discard` y@ parses both x and y, but discards the result of y
discard :: Parser t a -> Parser t b -> Parser t a
px `discard` py = do { x <- px; _ <- py; return x }

-- | @p `adjustErr` f@ applies the transformation @f@ to any error message
--   generated in @p@, having no effect if @p@ succeeds.
adjustErr :: Parser t a -> (String->String) -> Parser t a
(P p) `adjustErr` f = P (\ts-> case p ts of
                                 (Left msg, ts') -> (Left (f msg), ts')
                                 right           -> right )

-- | @adjustErrBad@ is just like @adjustErr@ except it also raises the
--   severity of the error.
adjustErrBad :: Parser t a -> (String->String) -> Parser t a
p `adjustErrBad` f = commit (p `adjustErr` f)

infixl 6 `onFail`	-- not sure about precedence 6?
-- | @p `onFail` q@ means parse p unless p fails in which case parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a *severe* failure in p cannot be ignored.
onFail :: Parser t a -> Parser t a -> Parser t a
(P p) `onFail` (P q) = P (\ts-> case p ts of
                                    (Left _, _) -> q ts
                                    right       -> right )

-- | Parse the first alternative in the list that succeeds.
oneOf :: [Parser t a] -> Parser t a
oneOf []     = do { n <- next
                  ; fail ("failed to parse any of the possible choices")
                  }
--oneOf :: Show t => [Parser t a] -> Parser t a
--oneOf []     = do { n <- next
--                  ; fail ("failed to parse any of the possible choices"
--                         ++"\n  next token is "++show n)
--                  }
oneOf (p:ps) = p `onFail` oneOf ps

-- | Parse the first alternative that succeeds, but if none succeed,
--   report only the severe errors, and if none of those, then report
--   all the soft errors.
oneOf' :: [(String, Parser t a)] -> Parser t a
oneOf' ps = accum [] ps
    where accum errs [] =
              case errs of
                [] ->  failBad ("internal failure in parser (oneOf'):\n"
                               ++indent 2 (show (map fst ps)))
                [(_,e)] -> fail e
                es -> fail ("one of the following failures occurred:\n"
                           ++indent 2 (concatMap showErr (reverse es)))
          accum errs ((e,P p):ps) =
              P (\ts-> case p ts of
                         (Left err,_) -> let (P p) = accum ((e,err):errs) ps
                                         in p ts
                         right        -> right )
          showErr (name,err) = name++":\n"++indent 2 err

-- | Helper for formatting error messages: indents all lines by a fixed amount.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | 'optional' indicates whether the parser succeeded through the Maybe type.
optional :: Parser t a -> Parser t (Maybe a)
optional p = fmap Just p `onFail` return Nothing

-- | 'many p' parses a list of elements with individual parser p.
--   Cannot fail, since an empty list is a valid return value.
many :: Parser t a -> Parser t [a]
many p = many1 p `onFail` return []

-- | Parse a non-empty list of items.
many1 :: Parser t a -> Parser t [a]
many1 p = do { x <- p `adjustErr` (("In a sequence:\n"++). indent 2)
             ; xs <- many p
             ; return (x:xs)
             }
--       `adjustErr` ("When looking for a non-empty sequence:\n\t"++)

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser t a -> Parser t sep -> Parser t [a]
sepBy p sep = do sepBy1 p sep `onFail` return []

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: Parser t a -> Parser t sep -> Parser t [a]
sepBy1 p sep = do { x <- p
                  ; xs <- many (do {sep; p})
                  ; return (x:xs)
                  }
         `adjustErr` ("When looking for a non-empty sequence with separators:\n\t"++)
 
-- | Parse a list of items, discarding the start, end, and separator
--   items.
bracketSep :: Parser t bra -> Parser t sep -> Parser t ket
              -> Parser t a -> Parser t [a]
bracketSep open sep close p =
    do { open; close; return [] }
       `onFail`
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; x <- p  `adjustErr` ("After first bracket in a group:\n\t"++)
       ; xs <- many (do {sep; p})
       ; close   `adjustErrBad` ("When looking for closing bracket:\n\t"++)
       ; return (x:xs)
       }

-- | Parse a bracketed item, discarding the brackets.
bracket :: Parser t bra -> Parser t ket -> Parser t a -> Parser t a
bracket open close p = do
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; x <- p
       ; close   `adjustErrBad` ("Missing closing bracket:\n\t"++)
       ; return x
       }

-- | 'manyFinally e t' parses a possibly-empty sequence of e's,
--   terminated by a t.  Any parse failures could be due either to
--   a badly-formed terminator or a badly-formed element, so raise
--   both possible errors.
manyFinally :: Parser t a -> Parser t z -> Parser t [a]
manyFinally pp@(P p) pt@(P t) = P (\ts ->
    case p ts of
        (Left e, _) ->
            case t ts of
                (Right _, ts') -> (Right [], ts')
                (Left e,  ts') -> (Left e,   ts')
        (Right x, ts') ->
            let (tail,ts'') = runParser (manyFinally pp pt) ts'
            in (Right (x:tail), ts'') )

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser t ()
reparse ts  = P (\inp-> (Right (), ts++inp))

------------------------------------------------------------------------
