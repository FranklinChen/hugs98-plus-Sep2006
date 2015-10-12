%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixFiles]{Haskell 1.3 POSIX File and Directory Operations}

\begin{code}
{-# OPTIONS -#include "HsPosix.h" #-}

module PosixFiles
    {-# DEPRECATED "This functionality is now available from System.Posix.Directory and System.Posix.Files" #-}
    (

    -- Directory streams
    DirStream,
    openDirStream, closeDirStream,
    readDirStream, rewindDirStream,

    -- set/get process' working directory.
    getWorkingDirectory, changeWorkingDirectory,

    -- File modes/permissions
    FileMode,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,

    unionFileModes, intersectFileModes,

    -- File operations on descriptors
    stdInput, stdOutput, stdError,
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openFd, createFile,

    -- other file&directory operations
    setFileCreationMask,
    createLink, removeLink,
    createDirectory, removeDirectory,
    createNamedPipe,
    rename,

    -- FileStatus
    FileStatus,
    getFileStatus, getFdStatus,
    fileExist,
    fileAccess,
    setFileMode,

    fileMode,
    fileID,         FileID,
    deviceID,       DeviceID,
    linkCount,
    fileOwner, fileGroup,
    fileSize,
    accessTime,     modificationTime, statusChangeTime,
    isDirectory,    isCharacterDevice,
    isBlockDevice,  isRegularFile,
    isNamedPipe,

    setOwnerAndGroup,  -- chown (might be restricted)
    setFileTimes,      -- set access and modification time
    touchFile,         -- set access and modification time to current time.

    -- run-time limit & POSIX feature testing
    PathVar(..),
    getPathVar,
    getFileVar

    ) where

import System.Posix
import System.Directory	( removeDirectory )

getFileVar = getFdPathVar
\end{code}
