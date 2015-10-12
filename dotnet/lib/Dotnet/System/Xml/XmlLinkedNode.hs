module Dotnet.System.Xml.XmlLinkedNode where

import Dotnet
import qualified Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlNodeTy

data XmlLinkedNode_ a
type XmlLinkedNode a = Dotnet.System.Xml.XmlNodeTy.XmlNode (XmlLinkedNode_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlLinkedNode.get_NextSibling"
  get_NextSibling :: XmlLinkedNode obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlLinkedNode.get_PreviousSibling"
  get_PreviousSibling :: XmlLinkedNode obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a0)


