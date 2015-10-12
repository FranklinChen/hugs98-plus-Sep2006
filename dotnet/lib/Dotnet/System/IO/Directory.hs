module Dotnet.System.IO.Directory where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.IO.DirectoryInfo
import Dotnet.System.DateTime
import Dotnet.System.Array
import Dotnet.System.String

data Directory_ a
type Directory a = Dotnet.System.Object.Object (Directory_ a)

getParent :: String -> Directory obj -> IO (Dotnet.System.IO.DirectoryInfo.DirectoryInfo a1)
getParent arg0  = invoke "GetParent" arg0
createDirectory :: String -> Directory obj -> IO (Dotnet.System.IO.DirectoryInfo.DirectoryInfo a1)
createDirectory arg0  = invoke "CreateDirectory" arg0
exists :: String -> Directory obj -> IO (Bool)
exists arg0  = invoke "Exists" arg0
setCreationTime :: String -> Dotnet.System.DateTime.DateTime a1 -> Directory obj -> IO (())
setCreationTime arg0 arg1  = invoke "SetCreationTime" (arg0,arg1)
getCreationTime :: String -> Directory obj -> IO (Dotnet.System.DateTime.DateTime a1)
getCreationTime arg0  = invoke "GetCreationTime" arg0
setLastWriteTime :: String -> Dotnet.System.DateTime.DateTime a1 -> Directory obj -> IO (())
setLastWriteTime arg0 arg1  = invoke "SetLastWriteTime" (arg0,arg1)
getLastWriteTime :: String -> Directory obj -> IO (Dotnet.System.DateTime.DateTime a1)
getLastWriteTime arg0  = invoke "GetLastWriteTime" arg0
setLastAccessTime :: String -> Dotnet.System.DateTime.DateTime a1 -> Directory obj -> IO (())
setLastAccessTime arg0 arg1  = invoke "SetLastAccessTime" (arg0,arg1)
getLastAccessTime :: String -> Directory obj -> IO (Dotnet.System.DateTime.DateTime a1)
getLastAccessTime arg0  = invoke "GetLastAccessTime" arg0
getFiles :: String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a1))
getFiles arg0  = invoke "GetFiles" arg0
getFiles_1 :: String -> String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a2))
getFiles_1 arg0 arg1  = invoke "GetFiles" (arg0,arg1)
getDirectories :: String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a1))
getDirectories arg0  = invoke "GetDirectories" arg0
getDirectories_1 :: String -> String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a2))
getDirectories_1 arg0 arg1  = invoke "GetDirectories" (arg0,arg1)
getFileSystemEntries :: String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a1))
getFileSystemEntries arg0  = invoke "GetFileSystemEntries" arg0
getFileSystemEntries_1 :: String -> String -> Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a2))
getFileSystemEntries_1 arg0 arg1  = invoke "GetFileSystemEntries" (arg0,arg1)
getLogicalDrives :: Directory obj -> IO (Dotnet.System.Array.Array (Dotnet.System.String.StringTy a0))
getLogicalDrives  = invoke "GetLogicalDrives" ()
getDirectoryRoot :: String -> Directory obj -> IO (String)
getDirectoryRoot arg0  = invoke "GetDirectoryRoot" arg0
getCurrentDirectory :: Directory obj -> IO (String)
getCurrentDirectory  = invoke "GetCurrentDirectory" ()
setCurrentDirectory :: String -> Directory obj -> IO (())
setCurrentDirectory arg0  = invoke "SetCurrentDirectory" arg0
move :: String -> String -> Directory obj -> IO (())
move arg0 arg1  = invoke "Move" (arg0,arg1)
delete :: String -> Directory obj -> IO (())
delete arg0  = invoke "Delete" arg0
delete_1 :: String -> Bool -> Directory obj -> IO (())
delete_1 arg0 arg1  = invoke "Delete" (arg0,arg1)

