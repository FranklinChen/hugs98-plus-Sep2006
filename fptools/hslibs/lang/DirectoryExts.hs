module DirectoryExts 
	{-# DEPRECATED "Use System.Directory instead" #-}
	( copyFile	-- :: FilePath -> FilePath -> IO ()
	) where

import System.Directory
