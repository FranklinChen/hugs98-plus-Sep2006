module Dotnet.System.TypeTy where

import qualified Dotnet ( Object )
import Dotnet.System.ObjectTy

data Type_ a
type Type a = Object (Type_ a)
