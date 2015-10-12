-----------------------------------------------------------------------------

A simple high-level interface to Regex

(c) Simon Marlow 1997-1999

Modified 1999 by Ian Jackson to fix an apparent fatal bug (?!)  and to
provide matchRegexAll.
-----------------------------------------------------------------------------

> module RegexString 
>	  {-# DEPRECATED "This module has moved to Text.Regex" #-}
>         ( Regex
>         , mkRegex             -- :: String -> Regex
>         , mkRegexWithOpts     -- :: String -> Bool -> Bool -> Regex

>         , matchRegex          -- :: Regex  -> String -> Maybe [String]
>         , matchRegexAll       -- :: Regex  
>                               -- -> String 
>                               -- -> Maybe ( String   -- before match
>                               --          , String   -- the matched string
>                               --          , String   -- after match
>                               --          , String   -- matched by last grouping
>                               --          , [String] -- $1 .. $n group matches
>                               --          )
> 
>         ) where

> import Text.Regex
