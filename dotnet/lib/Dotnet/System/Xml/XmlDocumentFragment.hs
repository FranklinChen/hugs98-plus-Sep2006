module Dotnet.System.Xml.XmlDocumentFragment where

import Dotnet
import qualified Dotnet.System.Xml.XmlNode
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlDocumentTy
import Dotnet.System.Xml.XmlNodeType

data XmlDocumentFragment_ a
type XmlDocumentFragment a = Dotnet.System.Xml.XmlNode.XmlNode (XmlDocumentFragment_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentFragment obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentFragment obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.set_InnerXml"
  set_InnerXml :: String -> XmlDocumentFragment obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_InnerXml"
  get_InnerXml :: XmlDocumentFragment obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_LocalName"
  get_LocalName :: XmlDocumentFragment obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.CloneNode"
  cloneNode :: Bool -> XmlDocumentFragment obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_OwnerDocument"
  get_OwnerDocument :: XmlDocumentFragment obj -> IO (Dotnet.System.Xml.XmlDocumentTy.XmlDocument a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_ParentNode"
  get_ParentNode :: XmlDocumentFragment obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_NodeType"
  get_NodeType :: XmlDocumentFragment obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentFragment.get_Name"
  get_Name :: XmlDocumentFragment obj -> IO (String)


