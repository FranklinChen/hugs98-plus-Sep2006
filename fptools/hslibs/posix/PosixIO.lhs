%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixIO]{Haskell 1.3 POSIX Input/Output Primitives}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

module PosixIO
    {-# DEPRECATED "This module has been superseded by System.Posix.IO" #-}
    (
    FdOption(..),
    FileLock,
    LockRequest(..),

    fdClose,
    createPipe,
    dup,
    dupTo,

    fdRead,
    fdWrite,
    fdSeek,

    queryFdOption,
    setFdOption,

    getLock,  setLock,
    waitToSetLock,

    -- Handle <-> Fd
    handleToFd, fdToHandle,
    ) where

import System.Posix

fdClose = closeFd
\end{code}

