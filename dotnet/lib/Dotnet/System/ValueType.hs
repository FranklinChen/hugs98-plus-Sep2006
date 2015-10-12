module Dotnet.System.ValueType where

import Dotnet
import qualified Dotnet.System.Object

data ValueType_ a
type ValueType a = Dotnet.System.Object.Object (ValueType_ a)

