module Dotnet.System.Collections.DictionaryEntry where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.ValueType

data DictionaryEntry_ a
type DictionaryEntry a = Dotnet.System.ValueType.ValueType (DictionaryEntry_ a)

foreign import dotnet
  "method System.Collections.DictionaryEntry.get_Key"
  get_Key :: DictionaryEntry obj -> IO (Dotnet.System.Object.Object a0)

foreign import dotnet
  "method System.Collections.DictionaryEntry.set_Key"
  set_Key :: Dotnet.System.Object.Object a0 -> DictionaryEntry obj -> IO (())

foreign import dotnet
  "method System.Collections.DictionaryEntry.get_Value"
  get_Value :: DictionaryEntry obj -> IO (Dotnet.System.Object.Object a0)

foreign import dotnet
  "method System.Collections.DictionaryEntry.set_Value"
  set_Value :: Dotnet.System.Object.Object a0 -> DictionaryEntry obj -> IO (())


