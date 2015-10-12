-- Create Debian entries for library packages bundled with the interpreter.
-- This program should be run in the top-level directory, and creates
--
--	debian/control
--	debian/libhugs-*-bundled.*

module Main where

import Control.Monad
import Data.Char
import Distribution.Compat.FilePath
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import System.Directory
import System.Environment
import System.IO

main = do
	copyFile "debian/control.in" "debian/control"
	args <- getArgs
	flip mapM_ args $ \ fname -> do
		pkg <- readPackageDescription fname
		debPackage fname pkg

debPackage :: FilePath -> PackageDescription -> IO ()
debPackage fname pkg =
	let
		virtual_name = debName (pkgName (package pkg))
		debian_name = virtual_name ++ "-bundled"
		showLine s = showString s . showChar '\n'
		depends = concat [debName name ++ ", " |
			Dependency name _ <- buildDepends pkg]
		desc = unlines $ map fmtLine $ lines $ description pkg
		fmtLine "" = " ."
		fmtLine s = " " ++ s
		(pkgDir, _) = splitFileName fname
	in do
	appendFile "debian/control" (showLine "" $
		showLine ("Package: " ++ debian_name) $
		showLine ("Architecture: any") $
		showLine ("Depends: hugs, " ++ depends ++ "${shlibs:Depends}") $
		showLine ("Section: devel") $
		showLine ("Priority: optional") $
		showLine ("Provides: " ++ virtual_name) $
		showLine ("Conflicts: " ++ virtual_name) $
		showLine ("Description: " ++ synopsis pkg) $
		showString desc $
		showLine (" .") $
		showLine (" This is the version bundled with the interpreter.") $
		"")
	writeFile ("debian/" ++ debian_name ++ ".install") $
		"usr/lib/hugs/packages/" ++ pkgName (package pkg) ++ "\n"
	writeFile ("debian/" ++ debian_name ++ ".README.Debian") $
		"Version of the " ++ pkgName (package pkg) ++
			" library package bundled with Hugs.\n\n" ++
		"Documentation for this package can be found in the ghc6-doc package.\n"
	when (not (null (licenseFile pkg))) $
		copyFile (pkgDir `joinFileName` licenseFile pkg)
			("debian/" ++ debian_name ++ ".copyright")
	examples <- doesDirectoryExist (pkgDir `joinFileName` "examples")
	when examples $ do
		writeFile ("debian/" ++ debian_name ++ ".examples") $
			"debian/tmp/usr/lib/hugs/demos/" ++ pkgName (package pkg) ++ "\n"

debName :: String -> String
debName name = "libhugs-" ++ map debianize name
  where debianize c
	  | isUpper c = toLower c
	  | isAlphaNum c = c
	  | otherwise = '-'
