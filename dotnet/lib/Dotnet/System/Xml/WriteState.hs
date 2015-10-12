module Dotnet.System.Xml.WriteState where

import Dotnet
import qualified Dotnet.System.Enum

data WriteState_ a
type WriteState a = Dotnet.System.Enum.Enum (WriteState_ a)


