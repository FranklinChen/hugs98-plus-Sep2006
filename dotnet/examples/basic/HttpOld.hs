--
-- Based on Mondrian example in DDJ.
--
module HttpOld where

import Dotnet 
import IOExts

--
-- NOTE: this version of the Http example accesses .NET
-- via primitive operations provided by the DotNet library
-- and not directly via the FFI. See Http.hs for a version
-- that does.
-- 

http :: String -> IO ()
http url = do
  req <- invokeStatic "System.Net.WebRequest" "Create" url
  if not (isNullObj req) then do
    rsp <- req # invoke "GetResponse" ()
    str <- rsp # invoke "GetResponseStream" ()
    ls  <- str # slurpString 
    putStrLn ls
   else
    putStrLn ("Unable to fetch "++ url)

slurpString :: Object a -> IO String
slurpString stream = do
  buf  <- mkVector ByteTy 200
  off  <- boxValue (0::Int)
  sz   <- boxValue (200::Int)
  let   
   go stream = do
     x    <- stream # invoke "Read" (buf, off, sz)
     stat <- hsValue x
     if (stat <= (0 :: Int))
      then return []
      else do
       ls <- bytesToUTF8 buf off x
       rs <- unsafeInterleaveIO (go stream)
       return (ls ++ rs)
  go stream

bytesToUTF8 :: Object a -> Object a -> Object a -> IO String
bytesToUTF8 byteArr off sz = do
  encUTF8 <- newObj "System.Text.UTF8Encoding" ()
  encUTF8 # invoke "GetString" (byteArr, off, sz)
  
