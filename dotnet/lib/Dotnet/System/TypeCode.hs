module Dotnet.System.TypeCode where

import Dotnet
import qualified Dotnet.System.Enum

data TypeCode_ a
type TypeCode a = Dotnet.System.Enum.Enum (TypeCode_ a)


