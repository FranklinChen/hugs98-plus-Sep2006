%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixTTY]{Haskell 1.3 POSIX Device-Specific Functions}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

module PosixTTY
    {-# DEPRECATED "This module has been superseded by System.Posix.Terminal" #-}
    (
    BaudRate(..),
    ControlCharacter(..),
    FlowAction(..),
    QueueSelector(..),
    TerminalAttributes,
    TerminalMode(..),
    TerminalState(..),
    bitsPerByte,
    controlChar,
    controlFlow,
    discardData,
    drainOutput,
    getTerminalAttributes,
    getTerminalProcessGroupID,
    inputSpeed,
    inputTime,
    minInput,
    outputSpeed,
    sendBreak,
    setTerminalAttributes,
    setTerminalProcessGroupID,
    terminalMode,
    withBits,
    withCC,
    withInputSpeed,
    withMinInput,
    withMode,
    withOutputSpeed,
    withTime,
    withoutCC,
    withoutMode
    ) where

import System.Posix
\end{code}
