module Dotnet.System.Xml.XmlAttributeCollection 
	( module Dotnet.System.Xml.XmlAttributeCollection ,
	  module Dotnet.System.Xml.XmlAttributeCollectionTy
	) where

import Dotnet
import qualified Dotnet.System.Xml.XmlAttributeCollectionTy
import qualified Dotnet.System.Xml.XmlNamedNodeMap
import qualified Dotnet.System.Xml.XmlAttributeTy
import qualified Dotnet.System.Xml.XmlNodeTy
import qualified Dotnet.System.Array

data XmlAttributeCollection_ a
type XmlAttributeCollection a = Dotnet.System.Xml.XmlNamedNodeMap.XmlNamedNodeMap (XmlAttributeCollection_ a)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.RemoveAll"
  removeAll :: XmlAttributeCollection obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.RemoveAt"
  removeAt :: Int -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.Remove"
  remove :: Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.InsertAfter"
  insertAfter :: Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0 -> Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.InsertBefore"
  insertBefore :: Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0 -> Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.Append"
  append :: Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.Prepend"
  prepend :: Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf :: String -> String -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf_1 :: String -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.get_ItemOf"
  get_ItemOf_2 :: Int -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.SetNamedItem"
  setNamedItem :: Dotnet.System.Xml.XmlNodeTy.XmlNode a0 -> XmlAttributeCollection obj -> IO (Dotnet.System.Xml.XmlNodeTy.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlAttributeCollection.CopyTo"
  copyTo :: Dotnet.System.Array.Array (Dotnet.System.Xml.XmlAttributeTy.XmlAttribute a0) -> Int -> XmlAttributeCollection obj -> IO (())


