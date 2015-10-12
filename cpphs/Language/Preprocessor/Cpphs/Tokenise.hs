-----------------------------------------------------------------------------
-- |
-- Module      :  Tokenise
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- The purpose of this module is to lex a source file (language
-- unspecified) into tokens such that cpp can recognise a replaceable
-- symbol or macro-use, and do the right thing.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.Tokenise
  ( linesCpp
  , reslash
  , tokenise
  , WordStyle(..)
  , deWordStyle
  , parseMacroCall
  ) where

import Char
import Language.Preprocessor.Cpphs.HashDefine
import Language.Preprocessor.Cpphs.Position

-- | A Mode value describes whether to tokenise a la Haskell, or a la Cpp.
--   The main difference is that in Cpp mode we should recognise line
--   continuation characters.
data Mode = Haskell | Cpp

-- | linesCpp is, broadly speaking, Prelude.lines, except that
--   on a line beginning with a \#, line continuation characters are
--   recognised.  In a line continuation, the newline character is
--   preserved, but the backslash is not.
linesCpp :: String -> [String]
linesCpp  []                 = []
linesCpp (x:xs) | x=='#'     = tok Cpp     ['#'] xs
                | otherwise  = tok Haskell [] (x:xs)
  where
    tok Cpp   acc ('\\':'\n':ys)   = tok Cpp ('\n':acc) ys
    tok _     acc ('\n':'#':ys)    = reverse acc: tok Cpp ['#'] ys
    tok _     acc ('\n':ys)        = reverse acc: tok Haskell [] ys
    tok _     acc []               = reverse acc: []
    tok mode  acc (y:ys)           = tok mode (y:acc) ys

-- | Put back the line-continuation characters.
reslash :: String -> String
reslash ('\n':xs) = '\\':'\n':reslash xs
reslash (x:xs)    = x: reslash xs
reslash   []      = []

----
-- | Submodes are required to deal correctly with nesting of lexical
--   structures.
data SubMode = Any | Pred (Char->Bool) (Posn->String->WordStyle)
             | String Char | LineComment | NestComment Int
             | CComment

-- | Each token is classified as one of Ident, Other, or Cmd:
--   * Ident is a word that could potentially match a macro name.
--   * Cmd is a complete cpp directive (\#define etc).
--   * Other is anything else.
data WordStyle = Ident Posn String | Other String | Cmd (Maybe HashDefine)
  deriving (Eq,Show)
other _ s = Other s

deWordStyle :: WordStyle -> String
deWordStyle (Ident _ i) = i
deWordStyle (Other i)   = i
deWordStyle (Cmd _)     = "\n"

-- | tokenise is, broadly-speaking, Prelude.words, except that:
--    * the input is already divided into lines
--    * each word-like "token" is categorised as one of {Ident,Other,Cmd}
--    * \#define's are parsed and returned out-of-band using the Cmd variant
--    * All whitespace is preserved intact as tokens.
--    * C-comments are converted to white-space (depending on first param)
--    * Parens and commas are tokens in their own right.
--    * Any cpp line continuations are respected.
--   No errors can be raised.
--   The inverse of tokenise is (concatMap deWordStyle).
tokenise :: Bool -> Bool -> Bool -> [(Posn,String)] -> [WordStyle]
tokenise _     _    _     [] = []
tokenise strip ansi lang ((pos,str):pos_strs) =
    (if lang then haskell else plaintext) Any [] pos pos_strs str
 where
    -- rules to lex Haskell
  haskell :: SubMode -> String -> Posn -> [(Posn,String)]
             -> String -> [WordStyle]
  haskell Any acc p ls ('\n':'#':xs)      = emit acc $  -- emit "\n" $
                                            cpp Any haskell [] [] p ls xs
    -- warning: non-maximal munch on comment
  haskell Any acc p ls ('-':'-':xs)       = emit acc $
                                            haskell LineComment "--" p ls xs
  haskell Any acc p ls ('{':'-':xs)       = emit acc $
                                            haskell (NestComment 0) "-{" p ls xs
  haskell Any acc p ls ('/':'*':xs)|strip = emit acc $
                                            haskell CComment "  " p ls xs
  haskell Any acc p ls ('"':xs)           = emit acc $
                                            haskell (String '"') ['"'] p ls xs
  haskell Any acc p ls ('\'':xs)          = emit acc $
                                            haskell (String '\'') "'" p ls xs
  haskell Any acc p ls (x:xs) | single x  = emit acc $ emit [x] $
                                            haskell Any [] p ls xs
  haskell Any acc p ls (x:xs) | space x   = emit acc $
                                            haskell (Pred space other) [x]
                                                                        p ls xs
  haskell Any acc p ls (x:xs) | symbol x  = emit acc $
                                            haskell (Pred symbol other) [x]
                                                                        p ls xs
 -- haskell Any [] p ls (x:xs) | ident0 x  = id $
  haskell Any acc p ls (x:xs) | ident0 x  = emit acc $
                                            haskell (Pred ident1 Ident) [x]
                                                                        p ls xs
  haskell Any acc p ls (x:xs)             = haskell Any (x:acc) p ls xs

  haskell pre@(Pred pred ws) acc p ls (x:xs)
                        | pred x    = haskell pre (x:acc) p ls xs
                        | otherwise = ws p (reverse acc):
                                      haskell Any [] p ls (x:xs)
  haskell (Pred _ ws) acc p [] []   = ws p (reverse acc): []
  haskell (String c) acc p ls ('\\':x:xs)
                        | x=='\\'   = haskell (String c) ('\\':'\\':acc) p ls xs
                        | x==c      = haskell (String c) (c:'\\':acc) p ls xs
  haskell (String c) acc p ls (x:xs)
                        | x==c      = emit (c:acc) $ haskell Any [] p ls xs
                        | otherwise = haskell (String c) (x:acc) p ls xs
  haskell LineComment acc p ls xs@('\n':_) = emit acc $ haskell Any [] p ls xs
  haskell LineComment acc p ls (x:xs)      = haskell LineComment (x:acc) p ls xs
  haskell (NestComment n) acc p ls ('{':'-':xs)
                                    = haskell (NestComment (n+1))
                                                            ("-{"++acc) p ls xs
  haskell (NestComment 0) acc p ls ('-':'}':xs)
                                    = emit ("}-"++acc) $ haskell Any [] p ls xs
  haskell (NestComment n) acc p ls ('-':'}':xs)
                                    = haskell (NestComment (n-1))
                                                            ("}-"++acc) p ls xs
  haskell (NestComment n) acc p ls (x:xs) = haskell (NestComment n) (x:acc)
                                                                        p ls xs
  haskell CComment acc p ls ('*':'/':xs)  = emit ("  "++acc) $
                                            haskell Any [] p ls xs
  haskell CComment acc p ls (_:xs)        = haskell CComment (' ':acc) p ls xs
  haskell mode acc _ ((p,l):ls) []        = haskell mode acc p ls ('\n':l)
  haskell _    acc _ [] []                = emit acc $ []

  -- rules to lex Cpp
  cpp :: SubMode -> (SubMode -> String -> Posn -> [(Posn,String)]
                     -> String -> [WordStyle])
         -> String -> [String] -> Posn -> [(Posn,String)]
         -> String -> [WordStyle]
  cpp mode next word line pos remaining input =
    lexcpp mode word line remaining input
   where
    lexcpp Any w l ls ('/':'*':xs)   = lexcpp (NestComment 0) "" (w*/*l) ls xs
    lexcpp Any w l ls ('/':'/':xs)   = lexcpp LineComment "  " (w*/*l) ls xs
    lexcpp Any w l ((p,l'):ls) ('\\':[])  = cpp Any next [] ("\n":w*/*l) p ls l'
    lexcpp Any w l ls ('\\':'\n':xs) = lexcpp Any [] ("\n":w*/*l) ls xs
    lexcpp Any w l ls xs@('\n':_)    = Cmd (parseHashDefine ansi
                                                           (reverse (w*/*l))):
                                       next Any [] pos ls xs
 -- lexcpp Any w l ls ('"':xs)     = lexcpp (String '"') ['"'] (w*/*l) ls xs
 -- lexcpp Any w l ls ('\'':xs)    = lexcpp (String '\'') "'"  (w*/*l) ls xs
    lexcpp Any w l ls ('"':xs)       = lexcpp Any [] ("\"":(w*/*l)) ls xs
    lexcpp Any w l ls ('\'':xs)      = lexcpp Any [] ("'": (w*/*l)) ls xs
    lexcpp Any [] l ls (x:xs)
                    | ident0 x  = lexcpp (Pred ident1 Ident) [x] l ls xs
 -- lexcpp Any w l ls (x:xs) | ident0 x  = lexcpp (Pred ident1 Ident) [x] (w*/*l) ls xs
    lexcpp Any w l ls (x:xs)
                    | single x  = lexcpp Any [] ([x]:w*/*l) ls xs
                    | space x   = lexcpp (Pred space other) [x] (w*/*l) ls xs
                    | symbol x  = lexcpp (Pred symbol other) [x] (w*/*l) ls xs
                    | otherwise = lexcpp Any (x:w) l ls xs
    lexcpp pre@(Pred pred _) w l ls (x:xs)
                    | pred x    = lexcpp pre (x:w) l ls xs
                    | otherwise = lexcpp Any [] (w*/*l) ls (x:xs)
    lexcpp      (Pred _ _) w l [] []      = lexcpp Any [] (w*/*l) [] "\n"
    lexcpp (String c) w l ls ('\\':x:xs)
                    | x=='\\'   = lexcpp (String c) ('\\':'\\':w) l ls xs
                    | x==c      = lexcpp (String c) (c:'\\':w) l ls xs
    lexcpp (String c) w l ls (x:xs)
                    | x==c      = lexcpp Any [] ((c:w)*/*l) ls xs
                    | otherwise = lexcpp (String c) (x:w) l ls xs
    lexcpp LineComment w l ((p,l'):ls) ('\\':[])
                             = cpp LineComment next [] (('\n':w)*/*l) pos ls l'
    lexcpp LineComment w l ls ('\\':'\n':xs)
                                = lexcpp LineComment [] (('\n':w)*/*l) ls xs
    lexcpp LineComment w l ls xs@('\n':_) = lexcpp Any w l ls xs
    lexcpp LineComment w l ls (_:xs)      = lexcpp LineComment (' ':w) l ls xs
    lexcpp (NestComment _) w l ls ('*':'/':xs)
                                          = lexcpp Any [] (w*/*l) ls xs
    lexcpp (NestComment n) w l ls (_:xs)  = lexcpp (NestComment n) (' ':w) l
                                                                        ls xs
    lexcpp mode w l ((p,l'):ls) []        = cpp mode next w l pos ls ('\n':l')
    lexcpp _    _ _ []          []        = []

    -- rules to lex non-Haskell, non-cpp text
  plaintext :: SubMode -> String -> Posn -> [(Posn,String)]
            -> String -> [WordStyle]
  plaintext Any acc p ls ('\n':'#':xs)  = emit acc $  -- emit "\n" $
                                          cpp Any plaintext [] [] p ls xs
  plaintext Any acc p ls ('/':'*':xs)|strip = emit acc $
                                              plaintext CComment "  " p ls xs
  plaintext Any acc p ls (x:xs) | single x  = emit acc $ emit [x] $
                                              plaintext Any [] p ls xs
  plaintext Any acc p ls (x:xs) | space x   = emit acc $
                                              plaintext (Pred space other) [x]
                                                                        p ls xs
  plaintext Any acc p ls (x:xs) | ident0 x  = emit acc $
                                              plaintext (Pred ident1 Ident) [x]
                                                                        p ls xs
  plaintext Any acc p ls (x:xs)             = plaintext Any (x:acc) p ls xs
  plaintext pre@(Pred pred ws) acc p ls (x:xs)
                                | pred x    = plaintext pre (x:acc) p ls xs
                                | otherwise = ws p (reverse acc):
                                              plaintext Any [] p ls (x:xs)
  plaintext (Pred _ ws) acc p [] []         = ws p (reverse acc): []
  plaintext CComment acc p ls ('*':'/':xs)  = emit ("  "++acc) $
                                              plaintext Any [] p ls xs
  plaintext CComment acc p ls (_:xs)    = plaintext CComment (' ':acc) p ls xs
  plaintext mode acc _ ((p,l):ls) []    = plaintext mode acc p ls ('\n':l)
  plaintext _    acc _ [] []            = emit acc $ []

  -- predicates for lexing Haskell.
  ident0 x = isAlpha x    || x `elem` "_`"
  ident1 x = isAlphaNum x || x `elem` "'_`"
  symbol x = x `elem` ":!#$%&*+./<=>?@\\^|-~"
  single x = x `elem` "(),[];{}"
  space  x = x `elem` " \t"
  -- emit a token (if there is one) from the accumulator
  emit ""  = id
  emit xs  = (Other (reverse xs):)
  -- add a reversed word to the accumulator
  "" */* l = l
  w */* l  = reverse w : l


-- | Parse a possible macro call, returning argument list and remaining input
parseMacroCall :: [WordStyle] -> Maybe ([String],[WordStyle])
parseMacroCall = call . skip
  where
    skip (Other x:xs) | all isSpace x = skip xs
    skip xss                          = xss
    call (Other "(":xs)   = (args (0::Int) [] [] . skip) xs
    call _                = Nothing
    args 0 w acc (Other ",":xs)   = args 0 [] (addone w acc) (skip xs)
    args n w acc (Other "(":xs)   = args (n+1) ("(":w) acc xs
    args 0 w acc (Other ")":xs)   = Just (reverse (addone w acc), xs)
    args n w acc (Other ")":xs)   = args (n-1) (")":w) acc xs
    args n w acc (Ident _ var:xs) = args n (var:w) acc xs
    args n w acc (Other var:xs)   = args n (var:w) acc xs
    args _ _ _   _                = Nothing
    addone w acc = concat (reverse (dropWhile (all isSpace) w)): acc

