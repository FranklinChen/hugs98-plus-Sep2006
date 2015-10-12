module Dotnet.System.Xml.XPath.XPathNavigator 
	( module Dotnet.System.Xml.XPath.XPathNavigatorTy,
	  module Dotnet.System.Xml.XPath.XPathNavigator
	) where

import Dotnet
import qualified Dotnet.System.Xml.XmlNodeOrder
import Dotnet.System.Xml.XPath.XPathNavigatorTy
import qualified Dotnet.System.Xml.XPath.XPathNodeIterator
import qualified Dotnet.System.Xml.XPath.XPathNodeType
import qualified Dotnet.System.Xml.XPath.XPathExpression
import qualified Dotnet.System.Object
import qualified Dotnet.System.Xml.XPath.XPathNamespaceScope
import qualified Dotnet.System.Xml.XmlNameTable

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.IsDescendant"
  isDescendant :: Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.ComparePosition"
  comparePosition :: Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0 -> XPathNavigator obj -> IO (Dotnet.System.Xml.XmlNodeOrder.XmlNodeOrder a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectAncestors"
  selectAncestors :: String -> String -> Bool -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a3)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectAncestors"
  selectAncestors_1 :: Dotnet.System.Xml.XPath.XPathNodeType.XPathNodeType a0 -> Bool -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a2)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectDescendants"
  selectDescendants :: String -> String -> Bool -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a3)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectDescendants"
  selectDescendants_1 :: Dotnet.System.Xml.XPath.XPathNodeType.XPathNodeType a0 -> Bool -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a2)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectChildren"
  selectChildren :: String -> String -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a2)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.SelectChildren"
  selectChildren_1 :: Dotnet.System.Xml.XPath.XPathNodeType.XPathNodeType a0 -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Select"
  select :: String -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Select"
  select_1 :: Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a0 -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Matches"
  matches :: String -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Matches"
  matches_1 :: Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Evaluate"
  evaluate :: String -> XPathNavigator obj -> IO (Dotnet.System.Object.Object a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Evaluate"
  evaluate_1 :: Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a0 -> Dotnet.System.Xml.XPath.XPathNodeIterator.XPathNodeIterator a1 -> XPathNavigator obj -> IO (Dotnet.System.Object.Object a2)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Evaluate"
  evaluate_2 :: Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a0 -> XPathNavigator obj -> IO (Dotnet.System.Object.Object a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Compile"
  compile :: String -> XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a1)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.IsSamePosition"
  isSamePosition :: Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToId"
  moveToId :: String -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveTo"
  moveTo :: Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToRoot"
  moveToRoot :: XPathNavigator obj -> IO (())

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToParent"
  moveToParent :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToFirstChild"
  moveToFirstChild :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_HasChildren"
  get_HasChildren :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToFirst"
  moveToFirst :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToPrevious"
  moveToPrevious :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToNext"
  moveToNext :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToNextNamespace"
  moveToNextNamespace :: Dotnet.System.Xml.XPath.XPathNamespaceScope.XPathNamespaceScope a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToFirstNamespace"
  moveToFirstNamespace :: Dotnet.System.Xml.XPath.XPathNamespaceScope.XPathNamespaceScope a0 -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToNamespace"
  moveToNamespace :: String -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.GetNamespace"
  getNamespace :: String -> XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToNextAttribute"
  moveToNextAttribute :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToFirstAttribute"
  moveToFirstAttribute :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToAttribute"
  moveToAttribute :: String -> String -> XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.GetAttribute"
  getAttribute :: String -> String -> XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_HasAttributes"
  get_HasAttributes :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_NameTable"
  get_NameTable :: XPathNavigator obj -> IO (Dotnet.System.Xml.XmlNameTable.XmlNameTable a0)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_IsEmptyElement"
  get_IsEmptyElement :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_XmlLang"
  get_XmlLang :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_BaseURI"
  get_BaseURI :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_Value"
  get_Value :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_Prefix"
  get_Prefix :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_Name"
  get_Name :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_NamespaceURI"
  get_NamespaceURI :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_LocalName"
  get_LocalName :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.get_NodeType"
  get_NodeType :: XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNodeType.XPathNodeType a0)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.Clone"
  clone :: XPathNavigator obj -> IO (Dotnet.System.Xml.XPath.XPathNavigator.XPathNavigator a0)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.ToString"
  toString :: XPathNavigator obj -> IO (String)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToFirstNamespace"
  moveToFirstNamespace_1 :: XPathNavigator obj -> IO (Bool)

foreign import dotnet
  "method System.Xml.XPath.XPathNavigator.MoveToNextNamespace"
  moveToNextNamespace_1 :: XPathNavigator obj -> IO (Bool)


