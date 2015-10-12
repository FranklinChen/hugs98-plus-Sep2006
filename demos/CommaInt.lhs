This file contains the definition of commaint, a function which takes a
single string argument containing a sequence of digits, and outputs the
same sequence with commas inserted after every group of three digits,
reading from the right hand end of the string.

> module CommaInt where
>  commaint = reverse . foldr1 (\x y->x++","++y) . group 3 . reverse
>     where group n = takeWhile (not.null) . map (take n) . iterate (drop n)

This definition uses the following library functions:

  reverse, (.), foldr1, (++), takeWhile, not, null, map, take, iterate, drop.

Example: evaluation of commaint "1234567"

           "1234567"
               |
               | reverse
               V
           "7654321" _______________________________
               |                                    \
               | iterate (drop 3)                    |
               V                                     |
           ["7654321", "4321", "1", "", "", ...]     |
               |                                     |
               | map (take 3)                        V  group 3
               V                                     |
           ["765", "432", "1", "", "", ...]          |
               |                                     |
               | takeWhile (not.null)                |
               V     _______________________________/
           ["765", "432", "1"]
               |
               | foldr1 (\x y->x++","++y)
               V
           "765,432,1"
               |
               | reverse
               V
           "1,234,567"

In a Hugs session:

    ? commaint "1234567"
    1,234,567
    ?
























