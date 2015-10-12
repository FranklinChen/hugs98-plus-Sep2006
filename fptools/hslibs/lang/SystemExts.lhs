% -----------------------------------------------------------------------------
% $Id: SystemExts.lhs,v 1.9 2004/10/06 11:14:07 ross Exp $
%
% (c) The GHC Team, 2001
%

Systemy extensions.

\begin{code}
{-# OPTIONS -#include "HsLang.h" #-}
module SystemExts
	{-# DEPRECATED "This functionality is now available from System.Cmd and System.Environment" #-}
	( rawSystem,     -- :: String -> IO ExitCode

	, withArgs       -- :: [String] -> IO a -> IO a
	, withProgName   -- :: String -> IO a -> IO a
	
	, getEnvironment -- :: IO [(String, String)]
	
	) where

import System.Cmd
import System.Environment
\end{code}
