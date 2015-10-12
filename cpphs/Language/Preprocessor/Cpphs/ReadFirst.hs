-----------------------------------------------------------------------------
-- |
-- Module      :  ReadFirst
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Read the first file that matches in a list of search paths.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.ReadFirst
  ( readFirst
  ) where

import IO        (hPutStrLn, stderr)
import Directory (doesFileExist)
import List      (intersperse)
import Language.Preprocessor.Cpphs.Position  (Posn,directory)
import Language.Preprocessor.Cpphs.SymTab    (SymTab,lookupST)

-- | Attempt to read the given file from any location within the search path.
--   The first location found is returned, together with the file content.
--   (The directory of the calling file is always searched first, then
--    the current directory, finally any specified search path.)
readFirst :: String		-- ^ filename
	-> Posn			-- ^ inclusion point
	-> [String]		-- ^ search path
	-> SymTab String	-- ^ \#defined symbols
	-> IO ( FilePath
              , String
              )			-- ^ discovered filepath, and file contents

readFirst name demand path syms =
    try (cons dd (".":path))
  where
    dd = directory demand
    cons x xs = if null x then xs else x:xs
    realname = real name syms
    try [] = do
        hPutStrLn stderr ("Warning: Can't find file \""++realname
                         ++"\" in directories\n\t"
                         ++concat (intersperse "\n\t" (cons dd (".":path)))
                         ++"\n  Asked for by: "++show demand)
        return ("missing file: "++realname,"")
    try (p:ps) = do
        let file = p++'/':realname
        ok <- doesFileExist file
        if not ok then try ps
          else do content <- readFile file
                  return (file,content)

real :: String -> SymTab String -> String
real name syms = case name of
                   ('"':ns) -> init ns
                   ('<':ns) -> init ns
                   _        -> case lookupST name syms of
                                 Nothing -> name
                                 Just f  -> real f syms

