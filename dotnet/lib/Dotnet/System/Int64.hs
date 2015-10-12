module Dotnet.System.Int64 where

import Dotnet
import qualified Dotnet.System.ValueType

data Int64_ a
type Int64 a = Dotnet.System.ValueType.ValueType (Int64_ a)
