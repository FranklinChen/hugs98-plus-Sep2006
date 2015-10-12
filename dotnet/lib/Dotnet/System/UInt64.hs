module Dotnet.System.UInt64 where

import Dotnet
import qualified Dotnet.System.ValueType

data UInt64_ a
type UInt64 a = Dotnet.System.ValueType.ValueType (UInt64_ a)
