%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixErr]{Haskell 1.3 POSIX Error Codes}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

module PosixErr
  {-# DEPRECATED "This functionality is now available from Foreign.C.Error" #-}
  (
  ErrorCode,
  getErrorCode, noError,
  argumentListTooLong, e2BIG,
  badFd, eBADF,
  brokenPipe, ePIPE,
  directoryNotEmpty, eNOTEMPTY,
  execFormatError, eNOEXEC,
  fileAlreadyExists, eEXIST,
  fileTooLarge, eFBIG,
  filenameTooLong, eNAMETOOLONG,
  improperLink, eXDEV,
  inappropriateIOControlOperation, eNOTTY,
  inputOutputError, eIO,
  interruptedOperation, eINTR,
  invalidArgument, eINVAL,
  invalidSeek, eSPIPE,
  isADirectory, eISDIR,
  noChildProcess, eCHILD,
  noLocksAvailable, eNOLCK,
  noSpaceLeftOnDevice, eNOSPC,
  noSuchOperationOnDevice, eNODEV,
  noSuchDeviceOrAddress, eNXIO,
  noSuchFileOrDirectory, eNOENT,
  noSuchProcess, eSRCH,
  notADirectory, eNOTDIR,
  notEnoughMemory, eNOMEM,
  operationNotImplemented, eNOSYS,
  operationNotPermitted, ePERM,
  permissionDenied, eACCES,
  readOnlyFileSystem, eROFS,
  resourceBusy, eBUSY,
  resourceDeadlockAvoided, eDEADLK,
  resourceTemporarilyUnavailable, eAGAIN,
  tooManyLinks, eMLINK,
  tooManyOpenFiles, eMFILE,
  tooManyOpenFilesInSystem, eNFILE,
 ) where

import GHC.IOBase
import Foreign.C.Error

type ErrorCode = Errno

getErrorCode :: IO ErrorCode
getErrorCode = getErrno

noError :: ErrorCode
noError = eOK

argumentListTooLong :: ErrorCode
argumentListTooLong = e2BIG

badFd :: ErrorCode
badFd = eBADF

brokenPipe :: ErrorCode
brokenPipe = ePIPE

directoryNotEmpty :: ErrorCode
directoryNotEmpty = eNOTEMPTY

execFormatError :: ErrorCode
execFormatError = eNOEXEC

fileAlreadyExists :: ErrorCode
fileAlreadyExists = eEXIST

fileTooLarge :: ErrorCode
fileTooLarge = eFBIG

filenameTooLong :: ErrorCode
filenameTooLong = eNAMETOOLONG

improperLink :: ErrorCode
improperLink = eXDEV

inappropriateIOControlOperation :: ErrorCode
inappropriateIOControlOperation = eNOTTY

inputOutputError :: ErrorCode
inputOutputError = eIO

interruptedOperation :: ErrorCode
interruptedOperation = eINTR

invalidArgument :: ErrorCode
invalidArgument = eINVAL

invalidSeek :: ErrorCode
invalidSeek = eSPIPE

isADirectory :: ErrorCode
isADirectory = eISDIR

noChildProcess :: ErrorCode
noChildProcess = eCHILD

noLocksAvailable :: ErrorCode
noLocksAvailable = eNOLCK

noSpaceLeftOnDevice :: ErrorCode
noSpaceLeftOnDevice = eNOSPC

noSuchOperationOnDevice :: ErrorCode
noSuchOperationOnDevice = eNODEV

noSuchDeviceOrAddress :: ErrorCode
noSuchDeviceOrAddress = eNXIO

noSuchFileOrDirectory :: ErrorCode
noSuchFileOrDirectory = eNOENT

noSuchProcess :: ErrorCode
noSuchProcess = eSRCH

notADirectory :: ErrorCode
notADirectory = eNOTDIR

notEnoughMemory :: ErrorCode
notEnoughMemory = eNOMEM

operationNotImplemented :: ErrorCode
operationNotImplemented = eNOSYS

operationNotPermitted :: ErrorCode
operationNotPermitted = ePERM

permissionDenied :: ErrorCode
permissionDenied = eACCES

readOnlyFileSystem :: ErrorCode
readOnlyFileSystem = eROFS

resourceBusy :: ErrorCode
resourceBusy = eBUSY

resourceDeadlockAvoided :: ErrorCode
resourceDeadlockAvoided = eDEADLK

resourceTemporarilyUnavailable :: ErrorCode
resourceTemporarilyUnavailable = eAGAIN

tooManyLinks :: ErrorCode
tooManyLinks = eMLINK

tooManyOpenFiles :: ErrorCode
tooManyOpenFiles = eMFILE

tooManyOpenFilesInSystem :: ErrorCode
tooManyOpenFilesInSystem = eNFILE
\end{code}
