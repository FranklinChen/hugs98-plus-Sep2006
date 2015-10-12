module Dotnet.System.IO.TextWriter where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import qualified Dotnet.System.Array
import qualified Dotnet.System.Object
import qualified Dotnet.System.Decimal
import qualified Data.Word
import qualified Data.Int
import qualified Dotnet.System.Text.Encoding
import qualified Dotnet.System.IFormatProvider

data TextWriter_ a
type TextWriter a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (TextWriter_ a)

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine :: String -> Dotnet.System.Array.Array (Dotnet.System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_1 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> Dotnet.System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_2 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_3 :: String -> Dotnet.System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_4 :: Dotnet.System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_6 :: Dotnet.System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_7 :: Double -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_8 :: Double -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_9 :: Data.Word.Word64 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_10 :: Data.Int.Int64 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_11 :: Data.Word.Word32 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_14 :: Dotnet.System.Array.Array (Char) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_15 :: Dotnet.System.Array.Array (Char) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_17 :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write :: String -> Dotnet.System.Array.Array (Dotnet.System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_1 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> Dotnet.System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_2 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_3 :: String -> Dotnet.System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_4 :: Dotnet.System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_6 :: Dotnet.System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_7 :: Double -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_8 :: Double -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_9 :: Data.Word.Word64 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_10 :: Data.Int.Int64 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_11 :: Data.Word.Word32 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_14 :: Dotnet.System.Array.Array (Char) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_15 :: Dotnet.System.Array.Array (Char) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.set_NewLine"
  set_NewLine :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.get_NewLine"
  get_NewLine :: TextWriter obj -> IO (String)

foreign import dotnet
  "method System.IO.TextWriter.get_Encoding"
  get_Encoding :: TextWriter obj -> IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "method System.IO.TextWriter.Flush"
  flush :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Close"
  close :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.get_FormatProvider"
  get_FormatProvider :: TextWriter obj -> IO (Dotnet.System.IFormatProvider.IFormatProvider a0)

foreign import dotnet
  "static method System.IO.TextWriter.Synchronized"
  synchronized :: Dotnet.System.IO.TextWriter.TextWriter a0 -> IO (Dotnet.System.IO.TextWriter.TextWriter a1)

foreign import dotnet
  "static field System.IO.TextWriter.Null"
  get_Null :: IO (Dotnet.System.IO.TextWriter.TextWriter a0)


