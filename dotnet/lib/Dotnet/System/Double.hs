module Dotnet.System.Double where

import Dotnet
import qualified Dotnet.System.ValueType

data Double_ a
type Double a = Dotnet.System.ValueType.ValueType (Double_ a)
