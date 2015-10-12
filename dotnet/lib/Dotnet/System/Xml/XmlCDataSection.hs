module Dotnet.System.Xml.XmlCDataSection where

import Dotnet
import qualified Dotnet.System.Xml.XmlCharacterData
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNode
import Dotnet.System.Xml.XmlNodeType

data XmlCDataSection_ a
type XmlCDataSection a = Dotnet.System.Xml.XmlCharacterData.XmlCharacterData (XmlCDataSection_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlCDataSection obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlCDataSection obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.get_LocalName"
  get_LocalName :: XmlCDataSection obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.CloneNode"
  cloneNode :: Bool -> XmlCDataSection obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.get_NodeType"
  get_NodeType :: XmlCDataSection obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlCDataSection.get_Name"
  get_Name :: XmlCDataSection obj -> IO (String)


