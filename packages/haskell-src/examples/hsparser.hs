-- A simple test program for the Haskell parser,
-- originally written by Sven Panne.

module Main (main, mainArgs, testLexer) where

import Data.List
import Language.Haskell.Lexer (lexer, Token(EOF))
import Language.Haskell.ParseMonad (runParserWithMode)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import System.Environment
import System.Console.GetOpt

data Flag
	= LexOnlyLength          -- print number of tokens only
	| LexOnlyRev             -- print tokens in reverse order
	| LexOnly                -- print tokens
	| ParseLength            -- print number of declarations only
	| ParseInternal          -- print abstract syntax in internal format
	| ParsePretty PPLayout   -- pretty print in this style
	| Help                   -- give short usage info

title :: String
title = "A simple test program for the haskell-src package"

usage :: String
usage = "usage: hsparser [option] [filename]\n"

options :: [OptDescr Flag]
options =
   [ Option ['n']  ["numtokens"] (NoArg LexOnlyLength) "print number of tokens only",
     Option ['r']  ["revtokens"] (NoArg LexOnlyRev)    "print tokens in reverse order",
     Option ['t']  ["tokens"]    (NoArg LexOnly)       "print tokens",
     Option ['d']  ["numdecls"]  (NoArg ParseLength)   "print number of declarations only",
     Option ['a']  ["abstract"]  (NoArg ParseInternal) "print abstract syntax in internal format",
     Option ['p']  ["pretty"]    (OptArg pStyle "STYLE") "pretty print in STYLE[(o)ffside|(s)emicolon|(i)nline|(n)one](default = offside)",
     Option ['h','?'] ["help"]   (NoArg Help)          "display this help and exit"]

pStyle :: Maybe String -> Flag
pStyle Nothing = ParsePretty PPOffsideRule
pStyle (Just s) = ParsePretty $ case s of
	"o"		-> PPOffsideRule
	"offside"	-> PPOffsideRule
	"s"		-> PPSemiColon
	"semicolon"	-> PPSemiColon
	"i"		-> PPInLine
	"inline"	-> PPInLine
	"n"		-> PPNoLayout
	"none"		-> PPNoLayout
	_		-> PPOffsideRule

main :: IO ()
main = do
	args <- getArgs
	mainArgs args

mainArgs :: [String] -> IO ()
mainArgs cmdline =
    case getOpt Permute options cmdline of
	(flags, args, [])     -> do
		inp <- case args of
			[]  -> getContents
			[f] -> readFile f
			_   -> error usage
		let parse_mode = case args of
			[]  -> defaultParseMode
			[f] -> defaultParseMode {parseFilename = f}
		putStrLn (handleFlag (getFlag flags) parse_mode inp)
	(_,     _,    errors) ->
		error (concat errors ++ usageInfo usage options)

getFlag :: [Flag] -> Flag
getFlag []  = ParsePretty PPOffsideRule
getFlag [f] = f
getFlag _   = error usage

handleFlag :: Flag -> ParseMode -> String -> String
handleFlag LexOnlyLength   parse_mode = show . length . testLexerRev parse_mode
handleFlag LexOnlyRev      parse_mode =
	concat . intersperse "\n" . map show . testLexerRev parse_mode
handleFlag LexOnly         parse_mode =
	concat . intersperse "\n" . map show . testLexer parse_mode
handleFlag ParseLength     parse_mode =
	show . modLength . testParser parse_mode
    where modLength (HsModule _ _ _ imp d) = length imp + length d
handleFlag ParseInternal parse_mode = show . testParser parse_mode
handleFlag (ParsePretty l) parse_mode =
	prettyPrintStyleMode style{lineLength=80} defaultMode{layout=l} .
		testParser parse_mode
handleFlag Help           _parse_mode = const $
	usageInfo (title ++ "\n" ++ usage) options

testLexerRev :: ParseMode -> String -> [Token]
testLexerRev parse_mode = getResult . runParserWithMode parse_mode (loop [])
    where loop toks = lexer $ \t -> case t of 
				EOF -> return toks
				_   -> loop (t:toks)

testLexer :: ParseMode -> String -> [Token]
testLexer parse_mode = reverse . testLexerRev parse_mode

testParser :: ParseMode -> String -> HsModule
testParser parse_mode = getResult . parseModuleWithMode parse_mode

getResult :: ParseResult a -> a
getResult (ParseOk a) = a
getResult (ParseFailed loc err) =
	error (srcFilename loc ++ ":" ++ show (srcLine loc) ++ ":" ++
		show (srcColumn loc) ++ ": " ++ err)
