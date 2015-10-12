%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[PosixDB]{Haskell 1.4 POSIX System Databases}

\begin{code}
module PosixDB
    {-# DEPRECATED "This module has been superseded by System.Posix.User" #-}
    (
    GroupEntry(..),
    UserEntry(..),

    getUserEntryForID,    -- :: UserID -> IO UserEntry
    getUserEntryForName,  -- :: String -> IO UserEntry

    getGroupEntryForID,   -- :: GroupID -> IO GroupEntry
    getGroupEntryForName  -- :: String -> IO GroupEntry

    ) where

import System.Posix
\end{code}
