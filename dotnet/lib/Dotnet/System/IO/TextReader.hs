module Dotnet.System.IO.TextReader where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import qualified Dotnet.System.Array

data TextReader_ a
type TextReader a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (TextReader_ a)

foreign import dotnet
  "method System.IO.TextReader.ReadLine"
  readLine :: TextReader obj -> IO (String)

foreign import dotnet
  "method System.IO.TextReader.ReadBlock"
  readBlock :: Dotnet.System.Array.Array (Char) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.ReadToEnd"
  readToEnd :: TextReader obj -> IO (String)

foreign import dotnet
  "method System.IO.TextReader.Read"
  read :: Dotnet.System.Array.Array (Char) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Read"
  read_1 :: TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Peek"
  peek :: TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Close"
  close :: TextReader obj -> IO (())

foreign import dotnet
  "static method System.IO.TextReader.Synchronized"
  synchronized :: Dotnet.System.IO.TextReader.TextReader a0 -> IO (Dotnet.System.IO.TextReader.TextReader a1)

foreign import dotnet
  "static field System.IO.TextReader.Null"
  get_Null :: IO (Dotnet.System.IO.TextReader.TextReader a0)


