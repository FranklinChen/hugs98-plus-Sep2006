-----------------------------------------------------------------------------
-- 
-- Module      :  POpen
-- Copyright   :  (c) The University of Glasgow 2002
--  		  (c) 2001-2002 Jens-Ulrik Holger Petersen
-- License     :  BSD-style
-- 
-- Maintainer  :  petersen@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires POSIX support from the OS)
--
-- $Id: POpen.hs,v 1.6 2004/10/06 09:36:44 ross Exp $
--
-- Convenient string input to and output from a subprocess
--
-----------------------------------------------------------------------------
--
-- Description
--
-- POpen provides a convenient way of sending string input to a
-- subprocess and reading output from it lazily.
-- 
-- It provides two functions popen and popenEnvDir.
-- 
-- * popen gives lazy output and error streams from a
--   subprocess command, and optionally can direct input from a
--   string to the process.
-- 
-- * popenEnvDir in addition lets one specify the environment
--   and directory in which to run the subprocess command.
--
-- This code is originally based on Posix.runProcess, but it
-- uses file descriptors and pipes internally instead of
-- handles and returns the output and error streams lazily as
-- strings and also the pid of forked process.

module POpen
	{-# DEPRECATED "This functionality is now available from System.Process" #-}
	(popen, popenEnvDir) where

import PosixIO		( createPipe, dupTo, fdClose, fdToHandle )
import PosixProcPrim	( executeFile, forkProcess )

import System.Posix	( ProcessID, Fd, ProcessID,
			  stdInput, stdOutput, stdError )
import System.Directory	( setCurrentDirectory )
import System.IO	( hGetContents, hPutStr, hClose )
import Data.Maybe	( fromJust, isJust )
import Control.Monad	( when )

popen :: FilePath			-- Command
      -> [String]			-- Arguments
      -> Maybe String			-- Input
      -> IO (String, String, ProcessID)	-- (stdout, stderr, pid)
popen path args inpt =
    popenEnvDir path args inpt Nothing Nothing

popenEnvDir :: FilePath				-- Command
	    -> [String]				-- Arguments
	    -> Maybe String			-- Input
	    -> Maybe [(String, String)]		-- Environment
	    -> Maybe FilePath 			-- Working directory    
	    -> IO (String, String, ProcessID)	-- (stdout, stderr, pid)
popenEnvDir path args inpt env dir =
    do
    inr <- if (isJust inpt)
	   then
	       do
	       (inr', inw) <- createPipe
	       hin <- fdToHandle inw
	       hPutStr hin $ fromJust inpt
	       hClose hin
	       return $ Just inr'
	   else
	       return Nothing
    (outr, outw) <- createPipe
    (errr, errw) <- createPipe
    p <- forkProcess (doTheBusiness inr outw errw)

    -- close other end of pipes in here
    when (isJust inr) $
        fdClose $ fromJust inr
    fdClose outw
    fdClose errw
    hout <- fdToHandle outr
    outstrm <- hGetContents hout
    herr <- fdToHandle errr
    errstrm <- hGetContents herr
    return (outstrm, errstrm , p)
    
    where
    doTheBusiness :: 
	Maybe Fd	    -- stdin
	-> Fd		    -- stdout
	-> Fd		    -- stderr
        -> IO ()
    doTheBusiness inr outw errw = 
	do
	maybeChangeWorkingDirectory dir
	when (isJust inr) $
	     (do dupTo (fromJust inr) stdInput; return ())
	dupTo outw stdOutput
	dupTo errw stdError
	executeFile path True args env
	-- for typing, should never actually run
	error "executeFile failed!"

maybeChangeWorkingDirectory :: Maybe FilePath -> IO ()
maybeChangeWorkingDirectory dir =
    case dir of
	     Nothing -> return ()
	     Just x  -> setCurrentDirectory x
