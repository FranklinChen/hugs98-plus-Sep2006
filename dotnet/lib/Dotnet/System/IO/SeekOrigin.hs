module Dotnet.System.IO.SeekOrigin where

import Dotnet
import qualified Dotnet.System.Enum

data SeekOrigin_ a
type SeekOrigin a = Dotnet.System.Enum.Enum (SeekOrigin_ a)


