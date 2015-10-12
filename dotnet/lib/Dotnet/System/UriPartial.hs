module Dotnet.System.UriPartial where

import Dotnet
import qualified Dotnet.System.Enum

data UriPartial_ a
type UriPartial a = Dotnet.System.Enum.Enum (UriPartial_ a)


