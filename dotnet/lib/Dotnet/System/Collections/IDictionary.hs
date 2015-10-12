module Dotnet.System.Collections.IDictionary where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Collections.IDictionaryEnumerator
import qualified Dotnet.System.Collections.ICollection

data IDictionary_ a
type IDictionary a = Dotnet.System.Object.Object (IDictionary_ a)

foreign import dotnet
  "method System.Collections.IDictionary.Remove"
  remove :: Dotnet.System.Object.Object a0 -> IDictionary obj -> IO (())

foreign import dotnet
  "method System.Collections.IDictionary.GetEnumerator"
  getEnumerator :: IDictionary obj -> IO (Dotnet.System.Collections.IDictionaryEnumerator.IDictionaryEnumerator a0)

foreign import dotnet
  "method System.Collections.IDictionary.get_IsFixedSize"
  get_IsFixedSize :: IDictionary obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IDictionary.get_IsReadOnly"
  get_IsReadOnly :: IDictionary obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IDictionary.Clear"
  clear :: IDictionary obj -> IO (())

foreign import dotnet
  "method System.Collections.IDictionary.Add"
  add :: Dotnet.System.Object.Object a0 -> Dotnet.System.Object.Object a1 -> IDictionary obj -> IO (())

foreign import dotnet
  "method System.Collections.IDictionary.Contains"
  contains :: Dotnet.System.Object.Object a0 -> IDictionary obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IDictionary.get_Values"
  get_Values :: IDictionary obj -> IO (Dotnet.System.Collections.ICollection.ICollection a0)

foreign import dotnet
  "method System.Collections.IDictionary.get_Keys"
  get_Keys :: IDictionary obj -> IO (Dotnet.System.Collections.ICollection.ICollection a0)

foreign import dotnet
  "method System.Collections.IDictionary.set_Item"
  set_Item :: Dotnet.System.Object.Object a0 -> Dotnet.System.Object.Object a1 -> IDictionary obj -> IO (())

foreign import dotnet
  "method System.Collections.IDictionary.get_Item"
  get_Item :: Dotnet.System.Object.Object a0 -> IDictionary obj -> IO (Dotnet.System.Object.Object a1)


