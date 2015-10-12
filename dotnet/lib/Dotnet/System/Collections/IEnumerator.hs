--
-- The IEnumerator interface
-- 
module Dotnet.System.Collections.IEnumerator where

import Dotnet
import qualified Dotnet.System.Object

data IEnumerator_ a
type IEnumerator a = Object (IEnumerator_ a)

-- ToDo: make this type-safe.

current :: Dotnet.System.Object.Object a -> IO (Dotnet.System.Object.Object b)
current = invoke "get_Current" ()

moveNext :: Dotnet.System.Object.Object a -> IO Bool
moveNext = invoke "MoveNext" ()

reset :: Dotnet.System.Object.Object a -> IO ()
reset = invoke "Reset" ()

