module Dotnet.System.MarshalByRefObject where

import Dotnet
import qualified Dotnet.System.Object

data MarshalByRefObject_ a
type MarshalByRefObject a = Dotnet.System.Object.Object (MarshalByRefObject_ a)
