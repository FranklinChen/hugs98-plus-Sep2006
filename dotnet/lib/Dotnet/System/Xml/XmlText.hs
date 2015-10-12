module Dotnet.System.Xml.XmlText where

import Dotnet
import qualified Dotnet.System.Xml.XmlCharacterData
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlNodeTy
import Dotnet.System.Xml.XmlNodeType

data XmlText_ a
type XmlText a = Dotnet.System.Xml.XmlCharacterData.XmlCharacterData (XmlText_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.SplitText"
  splitText :: Int -> XmlText obj -> IO (Dotnet.System.Xml.XmlText.XmlText a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlText obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlText obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.get_LocalName"
  get_LocalName :: XmlText obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.CloneNode"
  cloneNode :: Bool -> XmlText obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.get_NodeType"
  get_NodeType :: XmlText obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.set_Value"
  set_Value :: String -> XmlText obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.get_Value"
  get_Value :: XmlText obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlText.get_Name"
  get_Name :: XmlText obj -> IO (String)


