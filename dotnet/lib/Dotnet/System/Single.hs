module Dotnet.System.Single where

import Dotnet
import qualified Dotnet.System.ValueType

data Single_ a
type Single a = Dotnet.System.ValueType.ValueType (Single_ a)
