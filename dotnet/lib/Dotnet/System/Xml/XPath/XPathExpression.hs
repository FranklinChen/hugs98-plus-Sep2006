module Dotnet.System.Xml.XPath.XPathExpression where

import Dotnet
import qualified Dotnet.System.Xml.XPath.XPathResultType
import qualified Dotnet.System.Xml.XmlNamespaceManager
import qualified Dotnet.System.Object
import qualified Dotnet.System.Xml.XPath.XmlSortOrder
import qualified Dotnet.System.Xml.XPath.XmlCaseOrder
import qualified Dotnet.System.Xml.XPath.XmlDataType
import qualified Dotnet.System.Collections.IComparer

data XPathExpression_ a
type XPathExpression a = Dotnet.System.Object.Object (XPathExpression_ a)

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.get_ReturnType"
  get_ReturnType :: XPathExpression obj -> IO (Dotnet.System.Xml.XPath.XPathResultType.XPathResultType a0)

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.SetContext"
  setContext :: Dotnet.System.Xml.XmlNamespaceManager.XmlNamespaceManager a0 -> XPathExpression obj -> IO (())

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.Clone"
  clone :: XPathExpression obj -> IO (Dotnet.System.Xml.XPath.XPathExpression.XPathExpression a0)

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.AddSort"
  addSort :: Dotnet.System.Object.Object a0 -> Dotnet.System.Xml.XPath.XmlSortOrder.XmlSortOrder a1 -> Dotnet.System.Xml.XPath.XmlCaseOrder.XmlCaseOrder a2 -> String -> Dotnet.System.Xml.XPath.XmlDataType.XmlDataType a4 -> XPathExpression obj -> IO (())

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.AddSort"
  addSort_1 :: Dotnet.System.Object.Object a0 -> Dotnet.System.Collections.IComparer.IComparer a1 -> XPathExpression obj -> IO (())

foreign import dotnet
  "method System.Xml.XPath.XPathExpression.get_Expression"
  get_Expression :: XPathExpression obj -> IO (String)


