module Dotnet.System.Xml.XmlElementTy where

import Dotnet
import Dotnet.System.Xml.XmlLinkedNode

data XmlElement_ a
type XmlElement a = Dotnet.System.Xml.XmlLinkedNode.XmlLinkedNode (XmlElement_ a)

