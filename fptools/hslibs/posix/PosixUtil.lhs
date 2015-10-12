%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1999
%
\section[PosixUtil]{(Glasgow) Haskell POSIX utilities}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

module PosixUtil
{-# DEPRECATED "This functionality is now available from Foreign.Marshal.Error" #-}
where

import System.Posix.Types
import GlaExts
import GHC.IOBase  -- IOError representation
\end{code}

Now some local functions that shouldn't go outside this library.

Fail with a SystemError.  Normally, we do not try to re-interpret
POSIX error numbers, so most routines in this file will only fail
with SystemError.  The only exceptions are (1) those routines where
failure of some kind may be considered ``normal''...e.g. getpwnam()
for a non-existent user, or (2) those routines which do not set
errno.

\begin{code}
syserr :: String -> IO a
syserr str = ioException (IOError Nothing     -- ToDo: better
			      SystemError
			      str
			      ""
			      Nothing)

-- common templates for system calls

nonzero_error :: IO Int -> String -> IO ()
nonzero_error io err = do
    rc <- io
    if rc == 0
       then return ()
       else syserr err

minusone_error :: IO Int -> String -> IO ()
minusone_error io err = do
    rc <- io
    if rc /= -1
       then return ()
       else syserr err

\end{code}
