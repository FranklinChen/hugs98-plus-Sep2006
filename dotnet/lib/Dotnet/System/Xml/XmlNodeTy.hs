module Dotnet.System.Xml.XmlNodeTy where

import qualified Dotnet.System.Object

data XmlNode_ a
type XmlNode a = Dotnet.System.Object.Object (XmlNode_ a)
