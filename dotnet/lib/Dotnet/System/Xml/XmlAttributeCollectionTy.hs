module Dotnet.System.Xml.XmlAttributeCollectionTy where

import Dotnet.System.Xml.XmlNamedNodeMap

data XmlAttributeCollection_ a
type XmlAttributeCollection a = Dotnet.System.Xml.XmlNamedNodeMap.XmlNamedNodeMap (XmlAttributeCollection_ a)

