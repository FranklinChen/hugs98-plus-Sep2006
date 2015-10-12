module Dotnet.System.Collections.IDictionaryEnumerator where

import Dotnet
import qualified Dotnet.System.Collections.DictionaryEntry
import qualified Dotnet.System.Object

data IDictionaryEnumerator_ a
type IDictionaryEnumerator a = Dotnet.System.Object.Object (IDictionaryEnumerator_ a)

foreign import dotnet
  "method System.Collections.IDictionaryEnumerator.get_Entry"
  get_Entry :: IDictionaryEnumerator obj -> IO (Dotnet.System.Collections.DictionaryEntry.DictionaryEntry a0)

foreign import dotnet
  "method System.Collections.IDictionaryEnumerator.get_Value"
  get_Value :: IDictionaryEnumerator obj -> IO (Dotnet.System.Object.Object a0)

foreign import dotnet
  "method System.Collections.IDictionaryEnumerator.get_Key"
  get_Key :: IDictionaryEnumerator obj -> IO (Dotnet.System.Object.Object a0)


