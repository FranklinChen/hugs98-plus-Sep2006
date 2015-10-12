-----------------------------------------------------------------------------
-- |
-- Module      :  CppIfdef
-- Copyright   :  1999-2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Perform a cpp.first-pass, gathering \#define's and evaluating \#ifdef's.
-- and \#include's.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.CppIfdef
  ( cppIfdef	-- :: FilePath -> [(String,String)] -> [String] -> Bool -> Bool
		--      -> String -> [(Posn,String)]
  ) where


import Language.Preprocessor.Cpphs.SymTab
import Text.ParserCombinators.HuttonMeijer
-- import HashDefine
import Language.Preprocessor.Cpphs.Position  (Posn,newfile,newline,newlines,cppline,newpos)
import Language.Preprocessor.Cpphs.ReadFirst (readFirst)
import Language.Preprocessor.Cpphs.Tokenise  (linesCpp,reslash)
import Char      (isDigit)
import Numeric   (readHex,readOct,readDec)
import System.IO.Unsafe (unsafePerformIO)
import IO        (hPutStrLn,stderr)

-- | Run a first pass of cpp, evaluating \#ifdef's and processing \#include's,
--   whilst taking account of \#define's and \#undef's as we encounter them.
cppIfdef :: FilePath		-- ^ File for error reports
	-> [(String,String)]	-- ^ Pre-defined symbols and their values
	-> [String]		-- ^ Search path for \#includes
	-> Bool			-- ^ Leave \#define and \#undef in output?
	-> Bool			-- ^ Place \#line droppings in output?
	-> String		-- ^ The input file content
	-> [(Posn,String)]	-- ^ The file after processing (in lines)
cppIfdef fp syms search leave locat =
    cpp posn defs search leave locat Keep . (cppline posn:) . linesCpp
  where
    posn = newfile fp
    defs = foldr insertST emptyST syms
-- Notice that the symbol table is a very simple one mapping strings
-- to strings.  This pass does not need anything more elaborate, in
-- particular it is not required to deal with any parameterised macros.


-- | Internal state for whether lines are being kept or dropped.
--   In @Drop n b@, @n@ is the depth of nesting, @b@ is whether
--   we have already succeeded in keeping some lines in a chain of
--   @elif@'s
data KeepState = Keep | Drop Int Bool

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: Posn -> SymTab String -> [String] -> Bool -> Bool -> KeepState
       -> [String] -> [(Posn,String)]
cpp _ _ _ _ _ _ [] = []

cpp p syms path leave ln Keep (l@('#':x):xs) =
    let ws = words x
        cmd = head ws
        sym = head (tail ws)
        rest = tail (tail ws)
        val  = maybe "1" id (un rest)
        un v = if null v then Nothing else Just (unwords v)
        down = if definedST sym syms then (Drop 1 False) else Keep
        up   = if definedST sym syms then Keep else (Drop 1 False)
        keep str = if gatherDefined p syms str then Keep else (Drop 1 False)
        skipn cpp' p' syms' path' ud xs' =
            let n = 1 + length (filter (=='\n') l) in
            (if leave then ((p,reslash l):) else (replicate n (p,"") ++)) $
            cpp' (newlines n p') syms' path' leave ln ud xs'
    in case cmd of
	"define" -> skipn cpp p (insertST (sym,val) syms) path Keep xs
	"undef"  -> skipn cpp p (deleteST sym syms) path Keep xs
	"ifndef" -> skipn cpp p syms path  down xs
	"ifdef"  -> skipn cpp p syms path  up   xs
	"if"     -> skipn cpp p syms path (keep (unwords (tail ws))) xs
	"else"   -> skipn cpp p syms path (Drop 1 False) xs
	"elif"   -> skipn cpp p syms path (Drop 1 True) xs
	"endif"  -> skipn cpp p syms path  Keep xs
	"pragma" -> skipn cpp p syms path  Keep xs
        ('!':_)  -> skipn cpp p syms path Keep xs	-- \#!runhs scripts
	"include"-> let (inc,content) =
	                  unsafePerformIO (readFirst (unwords (tail ws))
                                                     p path syms)
	            in
		    cpp p syms path leave ln Keep (("#line 1 "++show inc)
                                                  : linesCpp content
                                                  ++ cppline p :"": xs)
	"warning"-> unsafePerformIO $ do
                       hPutStrLn stderr (l++"\nin "++show p)
                       return $ skipn cpp p syms path Keep xs
	"error"  -> error (l++"\nin "++show p)
	"line"   | all isDigit sym
	         -> (if ln then ((p,l):) else id) $
                    cpp (newpos (read sym) (un rest) p)
                        syms path leave ln Keep xs
	n | all isDigit n
	         -> (if ln then ((p,l):) else id) $
	            cpp (newpos (read n) (un (tail ws)) p)
                        syms path leave ln Keep xs
          | otherwise
	         -> unsafePerformIO $ do
                       hPutStrLn stderr ("Warning: unknown directive #"++n
                                        ++"\nin "++show p)
                       return $
                         ((p,l): cpp (newline p) syms path leave ln Keep xs)

cpp p syms path leave ln (Drop n b) (('#':x):xs) =
    let ws = words x
        cmd = head ws
        delse    | n==1 && b = Drop 1 b
                 | n==1      = Keep
                 | otherwise = Drop n b
        dend     | n==1      = Keep
                 | otherwise = Drop (n-1) b
        keep str | n==1      = if not b && gatherDefined p syms str then Keep
                               else (Drop 1) b
                 | otherwise = Drop n b
        skipn cpp' p' syms' path' ud xs' =
                 let n' = 1 + length (filter (=='\n') x) in
                 replicate n' (p,"")
                 ++ cpp' (newlines n' p') syms' path' leave ln ud xs'
    in
    if      cmd == "ifndef" ||
            cmd == "if"     ||
            cmd == "ifdef"  then  skipn cpp p syms path (Drop (n+1) b) xs
    else if cmd == "elif"   then  skipn cpp p syms path
                                                  (keep (unwords (tail ws))) xs
    else if cmd == "else"   then  skipn cpp p syms path delse xs
    else if cmd == "endif"  then  skipn cpp p syms path dend xs
    else skipn cpp p syms path (Drop n b) xs
	-- define, undef, include, error, warning, pragma, line

cpp p syms path leave ln Keep (x:xs) =
    let p' = newline p in seq p' $
    (p,x):  cpp p' syms path leave ln Keep xs
cpp p syms path leave ln d@(Drop _ _) (_:xs) =
    let p' = newline p in seq p' $
    (p,""): cpp p' syms path leave ln d xs


----
gatherDefined :: Posn -> SymTab String -> String -> Bool
gatherDefined p st inp =
  case papply (parseBoolExp st) inp of
    []      -> error ("Cannot parse #if directive in file "++show p)
    [(b,_)] -> b
    _       -> error ("Ambiguous parse for #if directive in file "++show p)

parseBoolExp :: SymTab String -> Parser Bool
parseBoolExp st =
  do  a <- parseExp1 st
      skip (string "||")
      b <- first (skip (parseBoolExp st))
      return (a || b)
  +++
      parseExp1 st

parseExp1 :: SymTab String -> Parser Bool
parseExp1 st =
  do  a <- parseExp0 st
      skip (string "&&")
      b <- first (skip (parseExp1 st))
      return (a && b)
  +++
      parseExp0 st

parseExp0 :: SymTab String -> Parser Bool
parseExp0 st =
  do  skip (string "defined")
      sym <- bracket (skip (char '(')) (skip (many1 alphanum)) (skip (char ')'))
      return (definedST sym st)
  +++
  do  bracket (skip (char '(')) (parseBoolExp st) (skip (char ')'))
  +++
  do  skip (char '!')
      a <- parseExp0 st
      return (not a)
  +++
  do  sym1 <- skip (many1 alphanum)
      op <- parseOp st
      sym2 <- skip (many1 alphanum)
      let val1 = convert sym1 st
      let val2 = convert sym2 st
      return (op val1 val2)
  +++
  do  sym <- skip (many1 alphanum)
      case convert sym st of
        0 -> return False
        _ -> return True
  where
    convert sym st' =
      case lookupST sym st' of
        Nothing  -> safeRead sym
        (Just a) -> safeRead a
    safeRead s =
      case s of
        '0':'x':s' -> number readHex s'
        '0':'o':s' -> number readOct s'
        _          -> number readDec s
    number rd s =
      case rd s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseOp :: SymTab String -> Parser (Integer -> Integer -> Bool)
parseOp _ =
  do  skip (string ">=")
      return (>=)
  +++
  do  skip (char '>')
      return (>)
  +++
  do  skip (string "<=")
      return (<=)
  +++
  do  skip (char '<')
      return (<)
  +++
  do  skip (string "==")
      return (==)
  +++
  do  skip (string "!=")
      return (/=)
