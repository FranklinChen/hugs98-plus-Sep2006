module Dotnet.System.Xml.XmlAttribute 
	( module Dotnet.System.Xml.XmlAttribute ,
	  module Dotnet.System.Xml.XmlAttributeTy
	) where

import Dotnet
import Dotnet.System.Xml.XmlAttributeTy
import Dotnet.System.Xml.XmlElement
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlDocument
import Dotnet.System.Xml.XmlNodeType

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_OwnerElement"
  get_OwnerElement :: XmlAttribute obj -> IO (Dotnet.System.Xml.XmlElement.XmlElement a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_Specified"
  get_Specified :: XmlAttribute obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_BaseURI"
  get_BaseURI :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.set_InnerXml"
  set_InnerXml :: String -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_InnerXml"
  get_InnerXml :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.set_InnerText"
  set_InnerText :: String -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_InnerText"
  get_InnerText :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_LocalName"
  get_LocalName :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.set_Prefix"
  set_Prefix :: String -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_Prefix"
  get_Prefix :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_NamespaceURI"
  get_NamespaceURI :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.CloneNode"
  cloneNode :: Bool -> XmlAttribute obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_OwnerDocument"
  get_OwnerDocument :: XmlAttribute obj -> IO (Dotnet.System.Xml.XmlDocument.XmlDocument a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_ParentNode"
  get_ParentNode :: XmlAttribute obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_NodeType"
  get_NodeType :: XmlAttribute obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.set_Value"
  set_Value :: String -> XmlAttribute obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_Value"
  get_Value :: XmlAttribute obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttribute.get_Name"
  get_Name :: XmlAttribute obj -> IO (String)


