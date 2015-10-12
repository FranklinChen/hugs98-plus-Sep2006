module Dotnet.System.Xml.XmlNode 
	( module Dotnet.System.Xml.XmlNode,
	  module Dotnet.System.Xml.XmlNodeTy
	) where

import Dotnet
import qualified Dotnet.System.Object
import Dotnet.System.Xml.XmlNodeTy
--import Dotnet.System.Xml.XPath.XPathNavigator
import Dotnet.System.Xml.XmlElementTy
import Dotnet.System.Xml.XmlWriter
import Dotnet.System.Xml.XmlDocumentTy
import Dotnet.System.Xml.XmlAttributeCollectionTy
import Dotnet.System.Xml.XmlNodeList
import Dotnet.System.Xml.XmlNodeType
import Dotnet.System.Xml.XmlNamespaceManager
import Dotnet.System.Collections.IEnumerator

{-
foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.CreateNavigator"
  createNavigator :: XmlNode obj -> IO (Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0)
-}

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Item"
  get_Item :: String -> String -> XmlNode obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Item"
  get_Item_1 :: String -> XmlNode obj -> IO (Dotnet.System.Xml.XmlElementTy.XmlElement a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.GetPrefixOfNamespace"
  getPrefixOfNamespace :: String -> XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.GetNamespaceOfPrefix"
  getNamespaceOfPrefix :: String -> XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.RemoveAll"
  removeAll :: XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.WriteContentTo"
  writeContentTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.WriteTo"
  writeTo :: Dotnet.System.Xml.XmlWriter.XmlWriter a0 -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_BaseURI"
  get_BaseURI :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.set_InnerXml"
  set_InnerXml :: String -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_InnerXml"
  get_InnerXml :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_OuterXml"
  get_OuterXml :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.set_InnerText"
  set_InnerText :: String -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_InnerText"
  get_InnerText :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.Clone"
  clone :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_IsReadOnly"
  get_IsReadOnly :: XmlNode obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_LocalName"
  get_LocalName :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.set_Prefix"
  set_Prefix :: String -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Prefix"
  get_Prefix :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_NamespaceURI"
  get_NamespaceURI :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.Supports"
  supports :: String -> String -> XmlNode obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.Normalize"
  normalize :: XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.CloneNode"
  cloneNode :: Bool -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_HasChildNodes"
  get_HasChildNodes :: XmlNode obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.AppendChild"
  appendChild :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.PrependChild"
  prependChild :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.RemoveChild"
  removeChild :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.ReplaceChild"
  replaceChild :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> Dotnet.System.Xml.XmlNode.XmlNode a1 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.InsertAfter"
  insertAfter :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> Dotnet.System.Xml.XmlNode.XmlNode a1 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.InsertBefore"
  insertBefore :: Dotnet.System.Xml.XmlNode.XmlNode a0 -> Dotnet.System.Xml.XmlNode.XmlNode a1 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_LastChild"
  get_LastChild :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_FirstChild"
  get_FirstChild :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_OwnerDocument"
  get_OwnerDocument :: XmlNode obj -> IO (Dotnet.System.Xml.XmlDocumentTy.XmlDocument a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Attributes"
  get_Attributes :: XmlNode obj -> IO (Dotnet.System.Xml.XmlAttributeCollectionTy.XmlAttributeCollection a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_NextSibling"
  get_NextSibling :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_PreviousSibling"
  get_PreviousSibling :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_ChildNodes"
  get_ChildNodes :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNodeList.XmlNodeList a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_ParentNode"
  get_ParentNode :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_NodeType"
  get_NodeType :: XmlNode obj -> IO (Dotnet.System.Xml.XmlNodeType.XmlNodeType a0)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.set_Value"
  set_Value :: String -> XmlNode obj -> IO (())

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Value"
  get_Value :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.get_Name"
  get_Name :: XmlNode obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.SelectSingleNode"
  selectSingleNode :: String -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.SelectSingleNode"
  selectSingleNode_1 :: String -> Dotnet.System.Xml.XmlNamespaceManager.XmlNamespaceManager a1 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNode.XmlNode a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.SelectNodes"
  selectNodes :: String -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNodeList.XmlNodeList a1)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.SelectNodes"
  selectNodes_1 :: String -> Dotnet.System.Xml.XmlNamespaceManager.XmlNamespaceManager a1 -> XmlNode obj -> IO (Dotnet.System.Xml.XmlNodeList.XmlNodeList a2)

foreign import dotnet
  "method Dotnet.System.Xml.XmlNode.GetEnumerator"
  getEnumerator :: XmlNode obj -> IO (Dotnet.System.Collections.IEnumerator.IEnumerator a0)


