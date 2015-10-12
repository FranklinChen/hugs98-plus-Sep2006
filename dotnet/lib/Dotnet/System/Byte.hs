module Dotnet.System.Byte where

import Dotnet
import qualified Dotnet.System.Object

data Byte_ a
type Byte a = Dotnet.System.Object.Object (Byte_ a)

