module Dotnet.System.Xml.XmlWhitespace where

import Dotnet
import qualified Dotnet.System.Xml.XmlCharacterData
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlNodeType

data XmlWhitespace_ a
type XmlWhitespace a = Dotnet.System.Xml.XmlCharacterData.XmlCharacterData (XmlWhitespace_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.get_LocalName"
  get_LocalName :: XmlWhitespace obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.CloneNode"
  cloneNode :: Bool -> XmlWhitespace obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.get_NodeType"
  get_NodeType :: XmlWhitespace obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.set_Value"
  set_Value :: String -> XmlWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.get_Value"
  get_Value :: XmlWhitespace obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlWhitespace.get_Name"
  get_Name :: XmlWhitespace obj -> IO (String)


