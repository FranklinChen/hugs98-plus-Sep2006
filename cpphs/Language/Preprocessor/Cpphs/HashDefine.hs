-----------------------------------------------------------------------------
-- |
-- Module      :  HashDefine
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- What structures are declared in a \#define.
-----------------------------------------------------------------------------
 
module Language.Preprocessor.Cpphs.HashDefine
  ( HashDefine(..)
  , ArgOrText(..)
  , expandMacro
  , parseHashDefine
  ) where

import Char (isSpace)
import List (intersperse)

data HashDefine
	= LineDrop
		{ name :: String }
	| SymbolReplacement
		{ name		:: String
		, replacement	:: String
		, linebreaks    :: Int
		}
	| MacroExpansion
		{ name		:: String
		, arguments	:: [String]
		, expansion	:: [(ArgOrText,String)]
		, linebreaks    :: Int
		}
    deriving (Eq,Show)

-- | 'smart' constructor to avoid warnings from ghc (undefined fields)
symbolReplacement :: HashDefine
symbolReplacement =
    SymbolReplacement
	 { name=undefined, replacement=undefined, linebreaks=undefined }

-- | Macro expansion text is divided into sections, each of which is classified
--   as one of three kinds: a formal argument (Arg), plain text (Text),
--   or a stringised formal argument (Str).
data ArgOrText = Arg | Text | Str deriving (Eq,Show)

-- | Expand an instance of a macro.
--   Precondition: got a match on the macro name.
expandMacro :: HashDefine -> [String] -> Bool -> String
expandMacro macro parameters layout =
    let env = zip (arguments macro) parameters
        replace (Arg,s)  = maybe (error "formal param") id (lookup s env)
        replace (Str,s)  = maybe (error "formal param") str (lookup s env)
        replace (Text,s) = if layout then s else filter (/='\n') s
        str s = '"':s++"\""
    in
    concatMap replace (expansion macro)

-- | Parse a \#define, or \#undef, ignoring other \# directives
parseHashDefine :: Bool -> [String] -> Maybe HashDefine
parseHashDefine ansi def = (command . skip) def
  where
    skip xss@(x:xs) | all isSpace x = skip xs
                    | otherwise     = xss
    skip    []      = []
    command ("line":xs)   = Just (LineDrop ("#line"++concat xs))
    command ("define":xs) = Just (((define . skip) xs) { linebreaks=count def })
    command ("undef":xs)  = Just (((undef  . skip) xs) { linebreaks=count def })
    command _             = Nothing
    undef  (sym:_)   = symbolReplacement { name=sym, replacement=sym }
    define (sym:xs)  = case {-skip-} xs of
                           ("(":ys) -> (macroHead sym [] . skip) ys
                           ys       -> symbolReplacement
                                           { name=sym
                                           , replacement=chop (skip ys) }
    macroHead sym args (",":xs) = (macroHead sym args . skip) xs
    macroHead sym args (")":xs) = MacroExpansion
                                    { name =sym , arguments = reverse args
                                    , expansion = classifyRhs args (skip xs)
                                    , linebreaks = undefined }
    macroHead sym args (var:xs) = (macroHead sym (var:args) . skip) xs
    macroHead sym args []       = error ("incomplete macro definition:\n"
                                        ++"  #define "++sym++"("
                                        ++concat (intersperse "," args))
    classifyRhs args ("#":x:xs)
                          | ansi &&
                            x `elem` args    = (Str,x): classifyRhs args xs
    classifyRhs args ("##":xs)
                          | ansi             = classifyRhs args xs
    classifyRhs args (word:xs)
                          | word `elem` args = (Arg,word): classifyRhs args xs
                          | otherwise        = (Text,word): classifyRhs args xs
    classifyRhs _    []                      = []
    count = length . filter (=='\n') . concat
    chop  = concat . reverse . dropWhile (all isSpace) . reverse

