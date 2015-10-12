module Dotnet.System.DateTime where

import Dotnet
import Dotnet.System.ValueType

data DateTime_ a
type DateTime a = ValueType (DateTime_ a)
