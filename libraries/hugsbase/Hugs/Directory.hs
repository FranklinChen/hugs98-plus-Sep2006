--
-- Hugs98 implementation of the Haskell 98 module, Directory.
--
module Hugs.Directory
	( Permissions ( readable     -- :: Permissions -> Bool
	              , writable     -- :: Permissions -> Bool
		      , executable   -- :: Permissions -> Bool
		      , searchable   -- :: Permissions -> Bool
		      )
	   -- instances: Eq, Ord, Read, Show
	, createDirectory	     -- :: FilePath -> IO ()

	, removeDirectory            -- :: FilePath -> IO ()
	, removeFile                 -- :: FilePath -> IO ()

	, renameDirectory            -- :: FilePath -> FilePath -> IO ()
	, renameFile		     -- :: FilePath -> FilePath -> IO ()

	, getDirectoryContents       -- :: FilePath -> IO [FilePath]

	, getCurrentDirectory        -- :: IO FilePath
	, setCurrentDirectory        -- :: FilePath -> IO ()

	, doesFileExist	             -- :: FilePath -> IO Bool
	, doesDirectoryExist	     -- :: FilePath -> IO Bool

	, getPermissions	     -- :: FilePath -> IO Permissions
	, setPermissions	     -- :: FilePath -> Permissions -> IO ()

	, getModificationTime 	     -- :: FilePath -> IO ClockTime
	) where

import System.Time ( ClockTime(..) )

data Permissions
 = Permissions 
   { readable     :: Bool
   , writable     :: Bool
   , executable   :: Bool
   , searchable   :: Bool
   } deriving (Eq, Ord, Read, Show)

{-
 This module is really just a wrapper for various directory
 and file-related system calls.
-}
primitive createDirectory :: FilePath -> IO ()

primitive removeFile :: FilePath -> IO ()
primitive removeDirectory :: FilePath -> IO ()

primitive renameFile :: FilePath -> FilePath -> IO ()
primitive renameDirectory :: FilePath -> FilePath -> IO ()

primitive setCurrentDirectory :: FilePath -> IO ()
primitive getCurrentDirectory :: IO FilePath

primitive doesDirectoryExist :: FilePath -> IO Bool
primitive doesFileExist :: FilePath -> IO Bool

getPermissions :: FilePath -> IO Permissions
getPermissions fpath = do
    (r,w,e,s) <- getPerms fpath
    return (Permissions{readable=r,writable=w,executable=e,searchable=s})

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions fpath perms = 
  setPerms fpath (readable perms)
  		 (writable perms)
		 (executable perms)
		 (searchable perms)

primitive getPerms :: FilePath -> IO (Bool,Bool,Bool,Bool)
primitive setPerms :: FilePath -> Bool -> Bool -> Bool -> Bool -> IO ()

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fpath = do
  ls <- getDirContents fpath
    -- it is easiest for the primitive to create the
    -- list of entries in the order in which they're
    -- read, so the resulting list will be back to front.
    -- Hence, list reversal is needed.
  return (reverse ls)

primitive getDirContents :: FilePath -> IO [FilePath]

getModificationTime :: FilePath -> IO ClockTime
getModificationTime fPath = do
  x <- getModTime fPath
  return (TOD (fromIntegral x) 0)

primitive getModTime :: FilePath -> IO Int
