%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[PosixProcPrim]{Haskell 1.3 POSIX Process Primitives}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

#include "ghcconfig.h"

module PosixProcPrim
    {-# DEPRECATED "This module has been superseded by System.Posix.Process and System.Posix.Env" #-}
    (
    Handler(..),
    SignalSet,
    Signal,
    ProcessStatus(..),

    addSignal,
#ifndef cygwin32_HOST_OS
    awaitSignal,
#endif
    backgroundRead,
    backgroundWrite,
    blockSignals,
#ifndef cygwin32_HOST_OS
    continueProcess,
#endif
    deleteSignal,
    emptySignalSet,
    executeFile,
    exitImmediately,
    floatingPointException,
    forkProcess,
    fullSignalSet,
    getAnyProcessStatus,
    getEnvVar,
    getEnvironment,
    getGroupProcessStatus,
    getPendingSignals,
    getProcessStatus,
    getSignalMask,
    illegalInstruction,
    inSignalSet,
    installHandler,
    internalAbort,
    keyboardSignal,
    keyboardStop,
    keyboardTermination,
    killProcess,
    lostConnection,
    nullSignal,
    openEndedPipe,
    processStatusChanged,
    queryStoppedChildFlag,
    raiseSignal,
    realTimeAlarm,
    removeEnvVar,
    scheduleAlarm,
    segmentationViolation,
    setEnvVar,
    setSignalMask,
    setStoppedChildFlag,
    sigABRT,
    sigALRM,
    sigCHLD,
#ifndef cygwin32_HOST_OS
    sigCONT,
#endif
    sigFPE,
    sigHUP,
    sigILL,
    sigINT,
    sigKILL,
    sigPIPE,
    sigQUIT,
    sigSEGV,
    sigSTOP,
    sigTERM,
    sigTSTP,
    sigTTIN,
    sigTTOU,
    sigUSR1,
    sigUSR2,
    signalProcess,
    signalProcessGroup,
    sleep,
    softwareStop,
    softwareTermination,
    unBlockSignals,
    userDefinedSignal1,
    userDefinedSignal2,

    ExitCode

    ) where

import System.Posix
import System.Posix.Env

import GHC.IOBase

import GlaExts

import Foreign

getEnvVar :: String -> IO String
getEnvVar name = do
  value <- System.Posix.Env.getEnv name
  case value of
    Nothing -> ioError (userError $ "getEnvVar: no such environment variable " ++ name)
    Just v -> return v

setEnvVar :: String -> String -> IO ()
setEnvVar name value = System.Posix.Env.setEnv name value True{-overwrite-}

removeEnvVar :: String -> IO ()
removeEnvVar = System.Posix.Env.unsetEnv

unBlockSignals = unblockSignals
\end{code}
