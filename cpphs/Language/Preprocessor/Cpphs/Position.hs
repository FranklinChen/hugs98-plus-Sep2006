-----------------------------------------------------------------------------
-- |
-- Module      :  Position
-- Copyright   :  2000-2004 Malcolm Wallace
-- Licence     :  LGPL
--
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  experimental
-- Portability :  All
--
-- Simple file position information, with recursive inclusion points.
-----------------------------------------------------------------------------

module Language.Preprocessor.Cpphs.Position
  ( Posn(..)
  , newfile
  , addcol, newline, tab, newlines, newpos
  , cppline
  , filename, lineno, directory
  ) where

-- | Source positions contain a filename, line, column, and an
--   inclusion point, which is itself another source position,
--   recursively.
data Posn = Pn String !Int !Int (Maybe Posn)
        deriving (Eq)

instance Show Posn where
      showsPrec _ (Pn f l c i) = showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n    used by  " .
                                               shows p )

-- | Constructor
newfile :: String -> Posn
newfile name = Pn name 1 1 Nothing

-- | Updates
addcol :: Int -> Posn -> Posn
addcol n (Pn f r c i) = Pn f r (c+n) i

newline, tab :: Posn -> Posn
--newline (Pn f r _ i) = Pn f (r+1) 1 i
newline (Pn f r _ i) = let r' = r+1 in r' `seq` Pn f r' 1 i
tab     (Pn f r c i) = Pn f r (((c`div`8)+1)*8) i

newlines :: Int -> Posn -> Posn
newlines n (Pn f r _ i) = Pn f (r+n) 1 i

newpos :: Int -> Maybe String -> Posn -> Posn
newpos r Nothing  (Pn f _ c i) = Pn f r c i
newpos r (Just ('"':f)) (Pn _ _ c i) = Pn (init f) r c i
newpos r (Just f)       (Pn _ _ c i) = Pn f r c i

-- | Projections

lineno    :: Posn -> Int
filename  :: Posn -> String
directory :: Posn -> FilePath

lineno    (Pn _ r _ _) = r
filename  (Pn f _ _ _) = f
directory (Pn f _ _ _) = dirname f


-- | cpp-style printing
cppline :: Posn -> String
cppline (Pn f r _ _) = "#line "++show r++" "++show f

                                                                                
-- | Strip non-directory suffix from file name (analogous to the shell
--   command of the same name).
dirname :: String -> String
dirname  = reverse . safetail . dropWhile (not.(`elem`"\\/")) . reverse
  where safetail [] = []
        safetail (_:x) = x

