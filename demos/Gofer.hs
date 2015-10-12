-----------------------------------------------------------------------------
-- Utility functions, for compatibility with Gofer prelude & Bird and Wadler:
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Gofer where

-- String formatting: -------------------------------------------------------

ljustify, rjustify, cjustify :: Int -> String -> String
ljustify n s                  = s ++ space (n - length s)
rjustify n s                  = space (n - length s) ++ s
cjustify n s                  = space halfm ++ s ++ space (m - halfm)
                                where m     = n - length s
                                      halfm = m `div` 2

space                        :: Int -> String
space n                       = copy n ' '

layn        :: [String] -> String
layn         = lay 1 where lay _ []     = []
                           lay n (x:xs) = rjustify 4 (show n) ++ ") "
                                           ++ x ++ "\n" ++ lay (n+1) xs

-- Misc. list utilities: ----------------------------------------------------

copy                :: Int -> a -> [a]
copy n x             = take n (repeat x)

merge               :: Ord a => [a] -> [a] -> [a]
merge []     ys      = ys
merge xs     []      = xs
merge (x:xs) (y:ys)
        | x <= y     = x : merge xs (y:ys)
        | otherwise  = y : merge (x:xs) ys

sort                :: Ord a => [a] -> [a]
sort                 = foldr insert []

insert              :: Ord a => a -> [a] -> [a]
insert x []          = [x]
insert x (y:ys)
        | x <= y     = x:y:ys
        | otherwise  = y:insert x ys

-- Other functions: ---------------------------------------------------------

fst3                :: (a,b,c) -> a
fst3 (x,_,_)         = x

snd3                :: (a,b,c) -> b
snd3 (_,x,_)         = x

thd3                :: (a,b,c) -> c
thd3 (_,_,x)         = x

-----------------------------------------------------------------------------
