module Hugs.Quote(Quote, quote, trim) where

import Data.List
import Data.Ratio(Ratio)

class Quote a where
    quote :: a -> String

instance Quote Char where
    quote c = [c]

instance Quote String where
    quote = id

instance Quote Bool where
    quote = show

instance Show a => Quote (Maybe a) where
    quote = show

instance Quote Int where
    quote = show

instance Quote Integer where
    quote = show

instance Quote Float where
    quote = show

instance Quote Double where
    quote = show

instance Integral a => Quote (Ratio a) where
    quote = show

-- trims off leading whitespace up to a common prefix,
-- making it easy to layout here docs indented so that
-- are not visually confusing (especially if you are doing
-- something like using here docs to generate Haskell code)

trim s = unlines' ls'
  where ls  = lines' s
	ls' = map (trimoff 0 n) ls
	n = case filter (/= 0) $ map (whitecount 0) ls of
	      [] -> 0
	      xs -> minimum xs

-- like the prelude functions, but preserve (lack of) trailing newline
lines' s    = let (l,s') = break ('\n'==) s
	      in l : case s' of []      -> []
				(_:s'') -> lines' s''
unlines' ss = concat $ intersperse "\n" ss

whitecount n []        = n
whitecount n (' ':cs)  = whitecount (n + 1) cs
whitecount n ('\t':cs) = whitecount (8 * ((n + 8) `div` 8)) cs
whitecount n _         = n

trimoff n m [] = []
trimoff n m cs | n >= m = cs
trimoff n m (' ' :cs) = trimoff (n + 1) m cs
trimoff n m ('\t':cs) = trimoff (8 * ((n + 8) `div` 8)) m cs
trimoff n m cs        = cs
