module Dotnet.System.Collections.IList where

import Dotnet
import qualified Dotnet.System.Object

data IList_ a
type IList a = Dotnet.System.Object.Object (IList_ a)

foreign import dotnet
  "method System.Collections.IList.RemoveAt"
  removeAt :: Int -> IList obj -> IO (())

foreign import dotnet
  "method System.Collections.IList.Remove"
  remove :: Dotnet.System.Object.Object a0 -> IList obj -> IO (())

foreign import dotnet
  "method System.Collections.IList.Insert"
  insert :: Int -> Dotnet.System.Object.Object a1 -> IList obj -> IO (())

foreign import dotnet
  "method System.Collections.IList.IndexOf"
  indexOf :: Dotnet.System.Object.Object a0 -> IList obj -> IO (Int)

foreign import dotnet
  "method System.Collections.IList.get_IsFixedSize"
  get_IsFixedSize :: IList obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IList.get_IsReadOnly"
  get_IsReadOnly :: IList obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IList.Clear"
  clear :: IList obj -> IO (())

foreign import dotnet
  "method System.Collections.IList.Contains"
  contains :: Dotnet.System.Object.Object a0 -> IList obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.IList.Add"
  add :: Dotnet.System.Object.Object a0 -> IList obj -> IO (Int)

foreign import dotnet
  "method System.Collections.IList.set_Item"
  set_Item :: Int -> Dotnet.System.Object.Object a1 -> IList obj -> IO (())

foreign import dotnet
  "method System.Collections.IList.get_Item"
  get_Item :: Int -> IList obj -> IO (Dotnet.System.Object.Object a1)


