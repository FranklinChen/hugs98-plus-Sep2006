module Dotnet.System.Collections.ICollection where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Array

data ICollection_ a
type ICollection a = Dotnet.System.Object.Object (ICollection_ a)

foreign import dotnet
  "method System.Collections.ICollection.get_IsSynchronized"
  get_IsSynchronized :: ICollection obj -> IO (Bool)

foreign import dotnet
  "method System.Collections.ICollection.get_SyncRoot"
  get_SyncRoot :: ICollection obj -> IO (Dotnet.System.Object.Object a0)

foreign import dotnet
  "method System.Collections.ICollection.get_Count"
  get_Count :: ICollection obj -> IO (Int)

foreign import dotnet
  "method System.Collections.ICollection.CopyTo"
  copyTo :: Dotnet.System.Array.Array a0 -> Int -> ICollection obj -> IO (())


