module Dotnet.System.Enum where

import Dotnet
import qualified Dotnet.System.ValueType
import qualified Dotnet.System.TypeTy

data Enum_ a
type Enum a = Dotnet.System.ValueType.ValueType (Enum_ a)

foreign import dotnet
  "static Dotnet.System.Enum.Parse"
  parse :: Dotnet.System.TypeTy.Type a -> String -> IO (Enum b)


