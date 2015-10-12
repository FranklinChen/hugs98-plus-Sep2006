{-
-- The main program for cpphs, a simple C pre-processor written in Haskell.

-- Copyright (c) 2004 Malcolm Wallace
-- This file is GPL, although the libraries it uses are either standard
-- Haskell'98 or distributed under the LGPL.
-}
module Language.Preprocessor.Cpphs.RunCpphs ( runCpphs ) where

import Language.Preprocessor.Cpphs.CppIfdef (cppIfdef)
import Language.Preprocessor.Cpphs.MacroPass(macroPass)
import Language.Preprocessor.Cpphs.Options(CpphsOption(..), parseOption)
import Language.Preprocessor.Unlit as Unlit (unlit)


runCpphs :: [CpphsOption] -> FilePath -> String -> IO String
runCpphs opts filename input = do
  let ds = [x | CpphsMacro x <- opts]
      is = [x | CpphsPath x <- opts]
      macro = not (CpphsNoMacro `elem` opts)
      locat = not (CpphsNoLine  `elem` opts)
      lang  = not (CpphsText    `elem` opts)
      strip =      CpphsStrip   `elem` opts
      ansi  =      CpphsAnsi    `elem` opts
      layout=      CpphsLayout  `elem` opts
      unlit =      CpphsUnlit   `elem` opts

  let pass1 = cppIfdef filename ds is macro locat input
      pass2 = macroPass ds strip ansi layout lang pass1
      result = if not macro then unlines (map snd pass1) else pass2
      pass3 = if unlit then Unlit.unlit filename result else result

  return pass3
