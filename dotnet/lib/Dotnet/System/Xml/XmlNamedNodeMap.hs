module Dotnet.System.Xml.XmlNamedNodeMap where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Collections.IEnumerator
import Dotnet.System.Xml.XmlNodeTy

data XmlNamedNodeMap_ a
type XmlNamedNodeMap a = Dotnet.System.Object.Object (XmlNamedNodeMap_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.GetEnumerator"
  getEnumerator :: XmlNamedNodeMap obj -> IO (Dotnet.System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.RemoveNamedItem"
  removeNamedItem :: String -> String -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.GetNamedItem"
  getNamedItem :: String -> String -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.Item"
  item :: Int -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.get_Count"
  get_Count :: XmlNamedNodeMap obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.RemoveNamedItem"
  removeNamedItem_1 :: String -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.SetNamedItem"
  setNamedItem :: Dotnet.System.Xml.XmlNodeTy.XmlNode a0 -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNamedNodeMap.GetNamedItem"
  getNamedItem_1 :: String -> XmlNamedNodeMap obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)


