--
-- Based on Mondrian example in DDJ, at least originally.
--
module Http where

import Dotnet 
import IOExts ( unsafeInterleaveIO )

--
-- This version of the Http example uses the FFI to access
-- .NET; see HttpOld.hs for an alternate (and older) approach
-- to .NET interop.
--

--
-- 'http url' dumps out the response from issuing a HTTP GET
-- request to URL 'url'.
--
http :: String -> IO ()
http url = do
  req <- createURL url
  if not (isNullObj req) then do
    rsp <- req # getResponse
    str <- rsp # getResponseStream
    ls  <- str # slurpString 
    putStrLn ls
   else
    putStrLn ("Unable to fetch "++ url)


-- 
-- Define the types representing the objects we're accessing here.
-- 
data WebRequest_ a
type WebRequest a = Object (WebRequest_ a)

data WebResponse_ a
type WebResponse a = Object (WebResponse_ a)

data Stream_ a
type Stream a = Object (Stream_ a)

data UTF8Encoding_ a
-- not correct (TextEncoding is the parent), but precise enough.
type UTF8Encoding a = Object (UTF8Encoding_ a)

--
-- Binding to the methods required.
--

foreign import dotnet
  "static System.Net.WebRequest.Create"
  createURL :: String -> IO (WebRequest ())

foreign import dotnet
  "method GetResponse"
  getResponse :: WebRequest a -> IO (WebResponse ())

foreign import dotnet
  "method GetResponseStream"
  getResponseStream :: WebResponse () -> IO (Stream a)

foreign import dotnet
  "method Read"
  readOffBytes :: Object a -> Int -> Int -> Stream this -> IO Int

foreign import dotnet
  "method GetString"
  getString :: Object a -> Int -> Int -> UTF8Encoding this -> IO String

slurpString :: Stream a -> IO String
slurpString stream = do
  buf     <- mkVector ByteTy 200
  encUTF8 <- new "System.Text.UTF8Encoding"
  let   
   bytesToUTF8 byteArr off sz = do
      encUTF8 # getString byteArr off sz

   go stream = do
     stat    <- stream # readOffBytes buf 0 200
     if (stat <= (0 :: Int))
	-- error of some sort, just break off.
      then return []
      else do
       ls <- bytesToUTF8 buf 0 stat
       rs <- unsafeInterleaveIO (go stream)
       return (ls ++ rs)
  go stream

  
