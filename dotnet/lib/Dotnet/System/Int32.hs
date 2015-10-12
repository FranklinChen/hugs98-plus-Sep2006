module Dotnet.System.Int32 where

import Dotnet
import qualified Dotnet.System.ValueType

data Int32_ a
type Int32 a = Dotnet.System.ValueType.ValueType (Int32_ a)
