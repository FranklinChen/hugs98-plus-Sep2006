module Dotnet.System.Xml.XmlEntityReference where

import Dotnet
import qualified Dotnet.System.Xml.XmlLinkedNode
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNode
import Dotnet.System.Xml.XmlNodeType

data XmlEntityReference_ a
type XmlEntityReference a = Dotnet.System.Xml.XmlLinkedNode.XmlLinkedNode (XmlEntityReference_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_BaseURI"
  get_BaseURI :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_IsReadOnly"
  get_IsReadOnly :: XmlEntityReference obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_LocalName"
  get_LocalName :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.CloneNode"
  cloneNode :: Bool -> XmlEntityReference obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_NodeType"
  get_NodeType :: XmlEntityReference obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.set_Value"
  set_Value :: String -> XmlEntityReference obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_Value"
  get_Value :: XmlEntityReference obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlEntityReference.get_Name"
  get_Name :: XmlEntityReference obj -> IO (String)


