{---------------------------------------------------------------
Daan Leijen (c) 2001.  daan@cs.uu.nl

$Revision: 1.1 $
$Author: ross $
$Date: 2003/07/31 17:45:36 $
---------------------------------------------------------------}
module Main where

import While( prettyWhileFromFile )

main  = prettyWhileFromFile "fib.wh"
