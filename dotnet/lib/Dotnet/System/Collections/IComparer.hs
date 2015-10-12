module Dotnet.System.Collections.IComparer where

import Dotnet
import qualified Dotnet.System.Object

data IComparer_ a
type IComparer a = Dotnet.System.Object.Object (IComparer_ a)

foreign import dotnet
  "method System.Collections.IComparer.Compare"
  compare :: Dotnet.System.Object.Object a0 -> Dotnet.System.Object.Object a1 -> IComparer obj -> IO (Int)


