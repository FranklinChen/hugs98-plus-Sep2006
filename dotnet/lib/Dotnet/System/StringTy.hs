module Dotnet.System.StringTy where

import qualified Dotnet ( Object )
import Dotnet.System.ObjectTy

data String_ a
type StringTy a = Object (String_ a)
