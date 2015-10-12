module Dotnet.System.UInt32 where

import Dotnet
import qualified Dotnet.System.ValueType

data UInt32_ a
type UInt32 a = Dotnet.System.ValueType.ValueType (UInt32_ a)
