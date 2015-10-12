module Dotnet.System.Xml.XmlNodeList where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Collections.IEnumerator
import Dotnet.System.Xml.XmlNodeTy

data XmlNodeList_ a
type XmlNodeList a = Dotnet.System.Object.Object (XmlNodeList_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNodeList.GetEnumerator"
  getEnumerator :: XmlNodeList obj -> IO (Dotnet.System.Collections.IEnumerator.IEnumerator a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNodeList.get_ItemOf"
  get_ItemOf :: Int -> XmlNodeList obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNodeList.get_Count"
  get_Count :: XmlNodeList obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNodeList.Item"
  item :: Int -> XmlNodeList obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)


