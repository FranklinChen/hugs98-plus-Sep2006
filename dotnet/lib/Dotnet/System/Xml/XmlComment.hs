module Dotnet.System.Xml.XmlComment where

import Dotnet
import qualified Dotnet.System.Xml.XmlCharacterData
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNode
import Dotnet.System.Xml.XmlNodeType

data XmlComment_ a
type XmlComment a = Dotnet.System.Xml.XmlCharacterData.XmlCharacterData (XmlComment_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlComment obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlComment obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.get_LocalName"
  get_LocalName :: XmlComment obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.CloneNode"
  cloneNode :: Bool -> XmlComment obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.get_NodeType"
  get_NodeType :: XmlComment obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlComment.get_Name"
  get_Name :: XmlComment obj -> IO (String)


