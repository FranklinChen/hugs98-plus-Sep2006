-----------------------------------------------------------------------------
-- |
-- Module      :  MacroPass
-- Copyright   :  2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Perform a cpp.second-pass, accumulating \#define's and \#undef's,
-- whilst doing symbol replacement and macro expansion.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.MacroPass
  ( macroPass
  , preDefine
  ) where

import Language.Preprocessor.Cpphs.HashDefine (HashDefine(..), expandMacro)
import Language.Preprocessor.Cpphs.Tokenise   (tokenise, WordStyle(..), parseMacroCall)
import Language.Preprocessor.Cpphs.SymTab     (SymTab, lookupST, insertST, emptyST)
import Language.Preprocessor.Cpphs.Position   (Posn, newfile, filename, lineno)
import System.IO.Unsafe (unsafePerformIO)
import Time       (getClockTime, toCalendarTime, formatCalendarTime)
import Locale     (defaultTimeLocale)

noPos :: Posn
noPos = newfile "preDefined"

-- | Walk through the document, replacing calls of macros with their expanded RHS.
macroPass :: [(String,String)]	-- ^ Pre-defined symbols and their values
          -> Bool		-- ^ Strip C-comments?
          -> Bool		-- ^ Accept \# and \## operators?
          -> Bool		-- ^ Retain layout in macros?
          -> Bool		-- ^ Input language (Haskell\/not)
          -> [(Posn,String)]	-- ^ The input file content
          -> String		-- ^ The file after processing
macroPass syms strip hashes layout language =
    safetail		-- to remove extra "\n" inserted below
    . concat
    . macroProcess layout language (preDefine hashes language syms)
    . tokenise strip hashes language
    . ((noPos,""):)	-- ensure recognition of "\n#" at start of file
  where
    safetail [] = []
    safetail (_:xs) = xs


-- | Turn command-line definitions (from @-D@) into 'HashDefine's.
preDefine :: Bool -> Bool -> [(String,String)] -> SymTab HashDefine
preDefine hashes lang defines =
    foldr (insertST.defval) emptyST defines
  where
    defval (s,d) =
        let (Cmd (Just hd):_) = tokenise True hashes lang
                                   [(noPos,"\n#define "++s++" "++d++"\n")]
        in (name hd, hd)


-- | Trundle through the document, one word at a time, using the WordStyle
--   classification introduced by 'tokenise' to decide whether to expand a
--   word or macro.  Encountering a \#define or \#undef causes that symbol to
--   be overwritten in the symbol table.  Any other remaining cpp directives
--   are discarded and replaced with blanks, except for \#line markers.
--   All valid identifiers are checked for the presence of a definition
--   of that name in the symbol table, and if so, expanded appropriately.
macroProcess :: Bool -> Bool -> SymTab HashDefine -> [WordStyle] -> [String]
macroProcess _ _ _         []                    = []
macroProcess y l st (Other x: ws)                = x:    macroProcess y l st ws
macroProcess y l st (Cmd Nothing: ws)            = "\n": macroProcess y l st ws
macroProcess y l st (Cmd (Just (LineDrop x)): ws)= "\n":x:macroProcess y l st ws
macroProcess layout lang st (Cmd (Just hd): ws)  =
    let n = 1 + linebreaks hd in
    replicate n "\n" ++ macroProcess layout lang (insertST (name hd, hd) st) ws
macroProcess layout lang st (Ident p x: ws) =
    case x of
      "__FILE__" -> show (filename p): macroProcess layout lang st ws
      "__LINE__" -> show (lineno p):   macroProcess layout lang st ws
      "__DATE__" -> formatCalendarTime defaultTimeLocale "\"%d %b %Y\""
                        (unsafePerformIO (getClockTime>>=toCalendarTime)):
                                       macroProcess layout lang st ws
      "__TIME__" -> formatCalendarTime defaultTimeLocale "\"%H:%M:%S\""
                        (unsafePerformIO (getClockTime>>=toCalendarTime)):
                                       macroProcess layout lang st ws
      _ ->
        case lookupST x st of
            Nothing -> x: macroProcess layout lang st ws
            Just hd ->
                case hd of
                    SymbolReplacement _ r _ ->
                        -- one-level expansion only:
                        -- r: macroProcess layout st ws
                        -- multi-level expansion:
                        let r' = if layout then r else filter (/='\n') r in
                        macroProcess layout lang st
                                     (tokenise True False lang [(p,r')]
                                      ++ ws)
                    MacroExpansion _ _ _ _  ->
                        case parseMacroCall ws of
                            Nothing -> x: macroProcess layout lang st ws
                            Just (args,ws') ->
                                if length args /= length (arguments hd) then
                                     x: macroProcess layout lang st ws
                                else -- one-level expansion only:
                                     -- expandMacro hd args layout:
                                     --         macroProcess layout st ws'
                                     -- multi-level expansion:
                                     macroProcess layout lang st
                                              (tokenise True False lang
                                                [(p,expandMacro hd args layout)]
                                               ++ ws')

