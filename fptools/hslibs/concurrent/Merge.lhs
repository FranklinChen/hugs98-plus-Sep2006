\begin{code}
module Merge
  {-# DEPRECATED "mergeIO and nmergeIO have moved to Control.Concurrent" #-} 
  (module Control.Concurrent) where
import Control.Concurrent (mergeIO, nmergeIO)
\end{code}
