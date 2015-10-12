module Text.ParserCombinators.PolyStateLazy
  ( -- * A Parser datatype parameterised on arbitrary token type and state type.
    --   Parsers do not return explicit failure.  An exception is raised
    --   instead.  This allows partial results to be returned before a
    --   full parse is complete.
    Parser(P)	-- datatype, instance of: Functor, Monad
  , runParser	-- :: Parser s t a -> s -> [t] -> (a, s, [t])
  , failBad	-- :: String -> Parser s t a
  , commit	-- :: Parser s t a -> Parser s t a
    -- * Combinators
    -- ** primitives
  , next	-- :: Parser s t t
  , satisfy	-- :: (t->Bool) -> Parser s t t
  , apply	-- :: Parser t (a->b) -> Parser s t a -> Parser s t b
  , discard	-- :: Parser s t a -> Parser s t b -> Parser s t a
    -- ** error-handling
  , adjustErr	-- :: Parser s t a -> (String->String) -> Parser s t a
  , adjustErrBad-- :: Parser s t a -> (String->String) -> Parser s t a
  , indent	-- :: Int -> String -> String
    -- ** choices
  , onFail	-- :: Parser s t a -> Parser s t a -> Parser s t a
  , oneOf	-- :: [Parser s t a] -> Parser s t a
  , oneOf'	-- :: [(String, Parser s t a)] -> Parser s t a
  , optional	-- :: Parser s t a -> Parser s t (Maybe a)
    -- ** sequences
  , many	-- :: Parser s t a -> Parser s t [a]
  , many1	-- :: Parser s t a -> Parser s t [a]
  , sepBy	-- :: Parser s t a -> Parser s t sep -> Parser s t [a]
  , sepBy1	-- :: Parser s t a -> Parser s t sep -> Parser s t [a]
  , bracketSep	-- :: Parser s t bra -> Parser s t sep -> Parser s t ket
                --    -> Parser s t a -> Parser s t [a]
  , bracket	-- :: Parser s t bra -> Parser s t ket -> Parser s t a
                --    -> Parser s t a
  , manyFinally	-- :: Parser s t a -> Parser s t z -> Parser s t [a]
    -- ** state-handling
  , stUpdate	-- :: (s->s) -> Parser s t ()
  , stQuery	-- :: (s->a) -> Parser s t a
  , stGet	-- :: Parser s t s
    -- ** re-parsing
  , reparse	-- :: [t] -> Parser s t ()
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
--   reporting and a running state.  It can be used for arbitrary token
--   types, not just String input.
newtype Parser s t a = P (s -> [t] -> (Either String a, s, [t]))

-- | A return type like Either, that distinguishes not only between
--   right and wrong answers, but also had gradations of wrongness.
--   Not used in this library.  !!!!!!!!!!!!!!!!!!!!!!!!!!
type EitherE a b = Either (Bool,a) b

-- | Apply a parser to an initial state and input token sequence.
--   The parser cannot return an error value explicitly, so errors
--   raise an exception.  Thus, results can be partial (lazily constructed,
--   but containing undefined).
runParser :: Parser s t a -> s -> [t] -> (a, s, [t])
runParser (P p) s =
    (\ (e,s,ts)-> (case e of {Left m->throwE m; Right x->x}, s, ts))
    . p s

instance Functor (Parser s t) where
    fmap f (P p) = P (\s ts-> case p s ts of
                                (Left msg, s', ts') -> (Left msg,    s', ts')
                                (Right x,  s', ts') -> (Right (f x), s', ts'))
instance Monad (Parser s t) where
    return x     = P (\s ts-> (Right x, s, ts))
    (P f) >>= g  = P (\s ts-> case f s ts of
                                (Left msg, s', ts') -> (Left msg, s', ts')
                                (Right x,  s', ts') -> let (P g') = g x
                                                       in g' s' ts')
    fail msg     = P (\s ts-> (Left msg, s, ts))

-- | Simple failure can be corrected, but when a simple fail is not strong
--   enough, use failBad for emphasis.  It guarantees parsing will terminate
--   with an exception.
failBad :: String -> Parser s t a
failBad msg      = P (\s ts-> (throwE msg, s, ts))

-- | Commit is a way of raising the severity of any errors found within
--   its argument.  Used in the middle of a parser definition, it means that
--   any operations prior to commitment fail softly, but after commitment,
--   they fail hard.
commit :: Parser s t a -> Parser s t a
commit (P p) = P (\s ts-> case p s ts of
                            (Left e, s', ts') -> (throwE e, s', ts')
                            right             -> right )

-- Combinators

-- | One token
next :: Parser s t t
next = P (\s ts-> case ts of
                    []  -> (Left "Ran out of input (EOF)", s, [])
                    (t:ts') -> (Right t, s, ts') )

-- | One token satifying a predicate
satisfy :: (t->Bool) -> Parser s t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }

infixl 3 `apply`
-- | Apply a parsed function to a parsed value
apply :: Parser s t (a->b) -> Parser s t a -> Parser s t b
--pf `apply` px = do { f <- pf; x <- px; return (f x) }
-- Needs to be lazier!  Must not force the argument value too early.
(P pf) `apply` (P px) = P (\s ts->
    case pf s ts of
      (Left msg, s', ts') -> (Left msg, s', ts')
      (Right f,  s', ts') -> let (x',s'',ts'') = px s' ts'
                                 x = case x' of
                                         Right x -> x
                                         Left e  -> throwE e
                             in (Right (f x), s'', ts''))

infixl 3 `discard`
-- | @x `discard` y@ parses both x and y, but discards the result of y
discard :: Parser s t a -> Parser s t b -> Parser s t a
px `discard` py = do { x <- px; _ <- py; return x }

-- | @p `adjustErr` f@ applies the transformation @f@ to any error message
--   generated in @p@, having no effect if @p@ succeeds.
adjustErr :: Parser s t a -> (String->String) -> Parser s t a
(P p) `adjustErr` f =
        P (\s ts-> case p s ts of
                     (Left msg, s', ts') -> (Left (f msg), s, ts')
                     right               -> right )

-- | @adjustErrBad@ is just like @adjustErr@ except it also raises the
--   severity of the error.
adjustErrBad :: Parser s t a -> (String->String) -> Parser s t a
-- p `adjustErrBad` f = commit (p `adjustErr` f)
(P p) `adjustErrBad` f =
        P (\s ts-> case p s ts of
                     (Left msg, s', ts') -> (throwE (f msg), s, ts')
                     right               -> right )

infixl 6 `onFail`	-- not sure about precedence 6?
-- | @p `onFail` q@ means parse p unless p fails in which case parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a *severe* failure in p cannot be ignored.
onFail :: Parser s t a -> Parser s t a -> Parser s t a
(P p) `onFail` (P q) = P (\s ts-> case p s ts of
                                    (Left _, _, _) -> q s ts
                                    right          -> right )

-- | Parse the first alternative in the list that succeeds.
oneOf :: [Parser s t a] -> Parser s t a
oneOf []     = fail ("Failed to parse any of the possible choices")
oneOf (p:ps) = p `onFail` oneOf ps

-- | Parse the first alternative that succeeds, but if none succeed,
--   report only the severe errors, and if none of those, then report
--   all the soft errors.
oneOf' :: [(String, Parser s t a)] -> Parser s t a
oneOf' ps = accum [] ps
    where accum errs [] =
              case errs of
                [] -> failBad ("internal failure in parser (oneOf'):\n"
                              ++indent 2 (show (map fst ps)))
                [(_,e)] -> fail e
                es -> fail ("one of the following failures occurred:\n"
                           ++indent 2 (concatMap showErr (reverse es)))
          accum errs ((e,P p):ps) =
              P (\u ts-> case p u ts of
                           (Left err,_,_) -> let (P p) = accum ((e,err):errs) ps
                                             in p u ts
                           right          -> right )
          showErr (name,err) = name++":\n"++indent 2 err

-- | Helper for formatting error messages: indents all lines by a fixed amount.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | 'optional' indicates whether the parser succeeded through the Maybe type.
optional :: Parser s t a -> Parser s t (Maybe a)
optional p = fmap Just p `onFail` return Nothing

-- | 'many p' parses a list of elements with individual parser p.
--   Cannot fail, since an empty list is a valid return value.
many :: Parser s t a -> Parser s t [a]
many p = many1 p `onFail` return []

-- | Parse a non-empty list of items.
many1 :: Parser s t a -> Parser s t [a]
many1 p = do { x <- p `adjustErr` (("In a sequence:\n"++). indent 2)
             ; xs <- many p
             ; return (x:xs)
             }
--       `adjustErr` ("When looking for a non-empty sequence:\n"++)

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser s t a -> Parser s t sep -> Parser s t [a]
sepBy p sep = do sepBy1 p sep `onFail` return []

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: Parser s t a -> Parser s t sep -> Parser s t [a]
sepBy1 p sep = do { x <- p
                  ; xs <- many (do {sep; p})
                  ; return (x:xs)
                  }
         `adjustErr` ("When looking for a non-empty sequence with separators:\n"++)
 
-- | Parse a list of items, discarding the start, end, and separator
--   items.
bracketSep :: Parser s t bra -> Parser s t sep -> Parser s t ket
              -> Parser s t a -> Parser s t [a]
bracketSep open sep close p =
    do { open; close; return [] }
       `onFail`
    do { open    `adjustErr` ("Missing opening bracket:\n"++)
       ; x <- p  `adjustErr` ("After first bracket in a group:\n"++)
       ; xs <- many (do {sep; p})
       ; close   `adjustErrBad` ("When looking for closing bracket:\n"++)
       ; return (x:xs)
       }

-- | Parse a bracketed item, discarding the brackets.
bracket :: Parser s t bra -> Parser s t ket -> Parser s t a -> Parser s t a
bracket open close p = do
    do { open    `adjustErr` ("Missing opening bracket:\n"++)
       ; x <- p
       ; close   `adjustErrBad` ("Missing closing bracket:\n"++)
       ; return x
       }

-- | 'manyFinally e t' parses a possibly-empty sequence of e's,
--   terminated by a t.  Any parse failures could be due either to
--   a badly-formed terminator or a badly-formed element, so raise
--   both possible errors.
manyFinally :: Parser s t a -> Parser s t z -> Parser s t [a]
manyFinally pp@(P p) pt@(P t) = P (\s ts ->
    case p s ts of
        (Left e, _, _) ->
            case t s ts of
                (Right _, s', ts') -> (Right [], s', ts')
                (Left e, s', ts')  -> (Left e, s', ts')
        (Right x, s', ts') ->
            let (tail,s'',ts'') = runParser (manyFinally pp pt) s' ts'
            in (Right (x:tail), s'', ts'') )

------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s t ()
stUpdate f  = P (\s ts-> (Right (), f s, ts))

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s t a
stQuery f   = P (\s ts-> (Right (f s), s, ts))

-- | Deliver the entire internal state.
stGet      :: Parser s t s
stGet       = P (\s ts-> (Right s, s, ts))

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser s t ()
reparse ts  = P (\s inp-> (Right (), s, ts++inp))

------------------------------------------------------------------------
