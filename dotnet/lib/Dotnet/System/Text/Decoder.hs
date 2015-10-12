module Dotnet.System.Text.Decoder where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Array
import Dotnet.System.Byte
import Dotnet.System.Char

data Decoder_ a
type Decoder a = Dotnet.System.Object.Object (Decoder_ a)

foreign import dotnet
  "method Dotnet.System.Text.Decoder.GetChars"
  getChars :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Dotnet.System.Array.Array (Dotnet.System.Char.Char a3) -> Int -> Decoder obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Decoder.GetCharCount"
  getCharCount :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Decoder obj -> IO (Int)


