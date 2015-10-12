%
% (c) The AQUA Project, Glasgow University, 1998
%

\section[NumExts]{Misc numeric bits}

\begin{code}
module NumExts
  {-# DEPRECATED "Use Numeric instead" #-} 
       (
         doubleToFloat   -- :: Double -> Float
       , floatToDouble   -- :: Float -> Double

       , showHex         -- :: Integral a => a -> ShowS
       , showOct         -- :: Integral a => a -> ShowS
       , showBin         -- :: Integral a => a -> ShowS

	 -- general purpose number->string converter.
       , showIntAtBase   -- :: Integral a 
			 -- => a		-- base
			 -- -> (a -> Char)      -- digit to char
			 -- -> a                -- number to show.
			 -- -> ShowS
       , showListWith    -- :: (a -> ShowS)
			 -- -> [a]
			 -- -> ShowS
       ) where

import Data.Char ( intToDigit )
import Numeric   ( showHex, showOct, showIntAtBase )
import Text.Show ( showListWith )
#ifdef __GLASGOW_HASKELL__
import GHC.Exts
#endif
\end{code}

\begin{code}
#ifdef __HUGS__
primitive doubleToFloat :: Double -> Float
primitive floatToDouble :: Float -> Double
#endif

#ifdef __GLASGOW_HASKELL__
doubleToFloat :: Double -> Float
floatToDouble :: Float -> Double

doubleToFloat (D# d#) = F# (double2Float# d#)
floatToDouble (F# f#) = D# (float2Double# f#)
#endif /* __GLASGOW_HASKELL__ */

showBin :: Integral a => a -> ShowS
showBin = showIntAtBase 2 (intToDigit.fromIntegral)
\end{code}
