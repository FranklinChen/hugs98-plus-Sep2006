module Dotnet.System.Xml.XmlSignificantWhitespace where

import Dotnet
import qualified Dotnet.System.Xml.XmlCharacterData
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlNodeType

data XmlSignificantWhitespace_ a
type XmlSignificantWhitespace a = Dotnet.System.Xml.XmlCharacterData.XmlCharacterData (XmlSignificantWhitespace_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.get_LocalName"
  get_LocalName :: XmlSignificantWhitespace obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.CloneNode"
  cloneNode :: Bool -> XmlSignificantWhitespace obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.get_NodeType"
  get_NodeType :: XmlSignificantWhitespace obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.set_Value"
  set_Value :: String -> XmlSignificantWhitespace obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.get_Value"
  get_Value :: XmlSignificantWhitespace obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlSignificantWhitespace.get_Name"
  get_Name :: XmlSignificantWhitespace obj -> IO (String)


