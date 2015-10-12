module Dotnet.System.Text.Encoder where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Array
import Dotnet.System.Char
import Dotnet.System.Byte

data Encoder_ a
type Encoder a = Dotnet.System.Object.Object (Encoder_ a)

foreign import dotnet
  "method Dotnet.System.Text.Encoder.GetBytes"
  getBytes :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a3) -> Int -> Bool -> Encoder obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Text.Encoder.GetByteCount"
  getByteCount :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> Bool -> Encoder obj -> IO (Int)


