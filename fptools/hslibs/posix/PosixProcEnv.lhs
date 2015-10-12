%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixProcEnv]{Haskell 1.3 POSIX Process Environment}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

#include "ghcconfig.h"

module PosixProcEnv
    {-# DEPRECATED "This module has been superseded by System.Posix.Process" #-}
    (
    ProcessTimes(..),
    SysVar(..),
    SystemID(..),
    createProcessGroup,
    createSession,
    epochTime,
#if !defined(cygwin32_HOST_OS)
    getControllingTerminalName,
#endif
    getEffectiveGroupID,
    getEffectiveUserID,
    getEffectiveUserName,
#if !defined(cygwin32_HOST_OS)
    getGroups,
#endif
    getLoginName,
    getParentProcessID,
    getProcessGroupID,
    getProcessID,
    getProcessTimes,
    getRealGroupID,
    getRealUserID,
    getSysVar,
    getSystemID,
    getTerminalName,
    joinProcessGroup,

    queryTerminal,

    setGroupID,
    setProcessGroupID,
    setUserID
    ) where

import System.Posix

\end{code}
