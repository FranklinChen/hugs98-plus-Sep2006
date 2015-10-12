module Dotnet.System.Xml.XmlDocumentTy where

import Dotnet
import Dotnet.System.Xml.XmlNodeTy

data XmlDocument_ a
type XmlDocument a = Dotnet.System.Xml.XmlNodeTy.XmlNode (XmlDocument_ a)

