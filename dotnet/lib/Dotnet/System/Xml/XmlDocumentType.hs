module Dotnet.System.Xml.XmlDocumentType where

import Dotnet
import qualified Dotnet.System.Xml.XmlLinkedNode
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNode
import Dotnet.System.Xml.XmlNodeType
import Dotnet.System.Xml.XmlNamedNodeMap

data XmlDocumentType_ a
type XmlDocumentType a = Dotnet.System.Xml.XmlLinkedNode.XmlLinkedNode (XmlDocumentType_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentType obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlDocumentType obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_IsReadOnly"
  get_IsReadOnly :: XmlDocumentType obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_LocalName"
  get_LocalName :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.CloneNode"
  cloneNode :: Bool -> XmlDocumentType obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_NodeType"
  get_NodeType :: XmlDocumentType obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_Name"
  get_Name :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_Entities"
  get_Entities :: XmlDocumentType obj -> IO (Dotnet.System.Xml.XmlNamedNodeMap.XmlNamedNodeMap a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_Notations"
  get_Notations :: XmlDocumentType obj -> IO (Dotnet.System.Xml.XmlNamedNodeMap.XmlNamedNodeMap a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_PublicId"
  get_PublicId :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_SystemId"
  get_SystemId :: XmlDocumentType obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlDocumentType.get_InternalSubset"
  get_InternalSubset :: XmlDocumentType obj -> IO (String)


