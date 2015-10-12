%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section{Packed strings}

This sits on top of the sequencing/arrays world, notably @ByteArray#@s.

Glorious hacking (all the hard work) by Bryan O'Sullivan.
\begin{code}
module PackedString
  {-# DEPRECATED "This module has moved to Data.PackedString" #-} 
  ( module Data.PackedString ) where
import Data.PackedString
\end{code}
