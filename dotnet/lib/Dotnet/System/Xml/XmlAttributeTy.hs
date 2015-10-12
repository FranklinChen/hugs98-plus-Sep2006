module Dotnet.System.Xml.XmlAttributeTy where

import Dotnet
import Dotnet.System.Xml.XmlNodeTy

data XmlAttribute_ a
type XmlAttribute a = Dotnet.System.Xml.XmlNodeTy.XmlNode (XmlAttribute_ a)

