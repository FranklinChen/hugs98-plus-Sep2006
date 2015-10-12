module Dotnet.System.Text.Encoding where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Array
import Dotnet.System.Byte
import Dotnet.System.Text.Encoder
import Dotnet.System.Text.Decoder
import Dotnet.System.Char

data Encoding_ a
type Encoding a = Dotnet.System.Object.Object (Encoding_ a)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetString"
  getString :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetString"
  getString_1 :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetMaxCharCount"
  getMaxCharCount :: Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetMaxByteCount"
  getMaxByteCount :: Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetEncoder"
  getEncoder :: Encoding obj -> IO (Dotnet.System.Text.Encoder.Encoder a0)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetDecoder"
  getDecoder :: Encoding obj -> IO (Dotnet.System.Text.Decoder.Decoder a0)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_CodePage"
  get_CodePage :: Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetChars"
  getChars :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Dotnet.System.Array.Array (Dotnet.System.Char.Char a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetChars"
  getChars_1 :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Char.Char a3))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetChars"
  getChars_2 :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Char.Char a1))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetCharCount"
  getCharCount :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetCharCount"
  getCharCount_1 :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetBytes"
  getBytes :: String -> Int -> Int -> Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetBytes"
  getBytes_1 :: String -> Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a1))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetBytes"
  getBytes_2 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetBytes"
  getBytes_3 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a3))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetBytes"
  getBytes_4 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a1))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetByteCount"
  getByteCount :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetByteCount"
  getByteCount_1 :: String -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetByteCount"
  getByteCount_2 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_IsMailNewsSave"
  get_IsMailNewsSave :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_IsMailNewsDisplay"
  get_IsMailNewsDisplay :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_IsBrowserSave"
  get_IsBrowserSave :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_IsBrowserDisplay"
  get_IsBrowserDisplay :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_WindowsCodePage"
  get_WindowsCodePage :: Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_WebName"
  get_WebName :: Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_HeaderName"
  get_HeaderName :: Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_EncodingName"
  get_EncodingName :: Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.get_BodyName"
  get_BodyName :: Encoding obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetPreamble"
  getPreamble :: Encoding obj -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0))

foreign import dotnet
  "method Dotnet.System.Text.Encoding.GetHashCode"
  getHashCode :: Encoding obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoding.Equals"
  equals :: Dotnet.System.Object.Object a0 -> Encoding obj -> IO (Bool)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.Convert"
  convert :: Dotnet.System.Text.Encoding.Encoding a0 -> Dotnet.System.Text.Encoding.Encoding a1 -> Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a2) -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a3))

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.Convert"
  convert_1 :: Dotnet.System.Text.Encoding.Encoding a0 -> Dotnet.System.Text.Encoding.Encoding a1 -> Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a2) -> Int -> Int -> IO (Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a5))

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.GetEncoding"
  getEncoding :: Int -> IO (Dotnet.System.Text.Encoding.Encoding a1)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.GetEncoding"
  getEncoding_1 :: String -> IO (Dotnet.System.Text.Encoding.Encoding a1)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_ASCII"
  get_ASCII :: IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_Default"
  get_Default :: IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_Unicode"
  get_Unicode :: IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_BigEndianUnicode"
  get_BigEndianUnicode :: IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_UTF7"
  get_UTF7 :: IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method Dotnet.System.Text.Encoding.get_UTF8"
  get_UTF8 :: IO (Dotnet.System.Text.Encoding.Encoding a0)


