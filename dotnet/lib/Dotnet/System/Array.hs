module Dotnet.System.Array where

import Dotnet
import qualified Dotnet.System.Object

data Array_ a

type Array elt = Dotnet.System.Object.Object (Array_ elt)

isFixedSize :: Array elt -> IO Bool
isFixedSize = invoke "get_IsFixedSize" ()

isReadOnly :: Array elt -> IO Bool
isReadOnly = invoke "get_IsReadOnly" ()

isSynchronized :: Array elt -> IO Bool
isSynchronized = invoke "get_IsSynchronized" ()

arrayLength :: Array elt -> IO Int
arrayLength = invoke "get_Length" ()

rank :: Array elt -> IO Int
rank = invoke "get_Rank" ()

-- syncRoot

getValue :: NetType elt => Int -> Array elt -> IO elt
getValue idx = invoke "GetValue" idx

