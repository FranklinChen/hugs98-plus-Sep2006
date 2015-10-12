module Dotnet.System.Xml.XmlNameTable where

import Dotnet
import qualified Dotnet.System.Object

data XmlNameTable_ a
type XmlNameTable a = Dotnet.System.Object.Object (XmlNameTable_ a)

add :: String -> XmlNameTable a -> IO String
add str = invoke "Add" str

get :: String -> XmlNameTable a -> IO String
get str = invoke "Get" str
