module Dotnet.System.IO.Stream where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import qualified Data.Word
import qualified Dotnet.System.Array
import qualified Data.Int
import qualified Dotnet.System.IO.SeekOrigin
{-
import qualified Dotnet.System.IAsyncResult
import qualified Dotnet.System.AsyncCallback
-}
import qualified Dotnet.System.Object

data Stream_ a
type Stream a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (Stream_ a)

foreign import dotnet
  "method System.IO.Stream.WriteByte"
  writeByte :: Data.Word.Word8 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Write"
  write :: Dotnet.System.Array.Array (Data.Word.Word8) -> Int -> Int -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.ReadByte"
  readByte :: Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.Read"
  read :: Dotnet.System.Array.Array (Data.Word.Word8) -> Int -> Int -> Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.SetLength"
  setLength :: Data.Int.Int64 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Seek"
  seek :: Data.Int.Int64 -> Dotnet.System.IO.SeekOrigin.SeekOrigin a1 -> Stream obj -> IO (Data.Int.Int64)

{-
foreign import dotnet
  "method System.IO.Stream.EndWrite"
  endWrite :: Dotnet.System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.BeginWrite"
  beginWrite :: Dotnet.System.Array.Array (Data.Word.Word8) -> Int -> Int -> Dotnet.System.AsyncCallback.AsyncCallback a3 -> Dotnet.System.Object.Object a4 -> Stream obj -> IO (Dotnet.System.IAsyncResult.IAsyncResult a5)

foreign import dotnet
  "method System.IO.Stream.EndRead"
  endRead :: Dotnet.System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.BeginRead"
  beginRead :: Dotnet.System.Array.Array (Data.Word.Word8) -> Int -> Int -> Dotnet.System.AsyncCallback.AsyncCallback a3 -> Dotnet.System.Object.Object a4 -> Stream obj -> IO (Dotnet.System.IAsyncResult.IAsyncResult a5)
-}
foreign import dotnet
  "method System.IO.Stream.Flush"
  flush :: Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Close"
  close :: Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.set_Position"
  set_Position :: Data.Int.Int64 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.get_Position"
  get_Position :: Stream obj -> IO (Data.Int.Int64)

foreign import dotnet
  "method System.IO.Stream.get_Length"
  get_Length :: Stream obj -> IO (Data.Int.Int64)

foreign import dotnet
  "method System.IO.Stream.get_CanWrite"
  get_CanWrite :: Stream obj -> IO (Bool)

foreign import dotnet
  "method System.IO.Stream.get_CanSeek"
  get_CanSeek :: Stream obj -> IO (Bool)

foreign import dotnet
  "method System.IO.Stream.get_CanRead"
  get_CanRead :: Stream obj -> IO (Bool)

foreign import dotnet
  "static field System.IO.Stream.Null"
  get_Null :: IO (Dotnet.System.IO.Stream.Stream a0)


