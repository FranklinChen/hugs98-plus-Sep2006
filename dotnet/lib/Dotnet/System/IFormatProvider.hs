module Dotnet.System.IFormatProvider where

import Dotnet
import Dotnet.System.Object

data IFormatProvider_ a
type IFormatProvider a = Dotnet.System.Object.Object (IFormatProvider_ a)

