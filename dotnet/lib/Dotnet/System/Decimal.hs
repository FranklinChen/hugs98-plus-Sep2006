module Dotnet.System.Decimal where

import Dotnet
import qualified Dotnet.System.ValueType

data Decimal_ a
type Decimal a = Dotnet.System.ValueType.ValueType (Decimal_ a)
